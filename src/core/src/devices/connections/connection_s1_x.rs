/// single_in S1Pre, single_out S1Post.
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{AcMx, WcMx};
use crate::connectivity;
use crate::connectivity::{Generator, Acceptor, PassiveAcceptor, ActiveAcceptor};
use crate::connectivity::s1_pre::{SingleInComponentS1Pre, FwdPreS1};
use crate::connectivity::s1_post::{SingleOutComponentS1Post, FwdPostS1};
use crate::operation::{Configurable, Runnable, RunningSet, Broadcast, PassiveDevice, RunMode};
use crate::operation::op_device::ConsecutivePassiveDevice;
use crate::populations::HoldDevices;
use crate::components::Linker;

pub struct ConnectionS1X {
    in_s1_pre: SingleInComponentS1Pre,
    out_s1_post: SingleOutComponentS1Post,
    value: i32,
}

impl Configurable for ConnectionS1X {
    fn config_mode(&mut self, mode: RunMode) {
        self.in_s1_pre.config_mode(mode);
        self.out_s1_post.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.in_s1_pre.config_channels();
        self.out_s1_post.config_channels();   
    }

    fn mode(&self) -> RunMode {
        RunMode::eq_mode(self.in_s1_pre.mode(),self.out_s1_post.mode())
    }
}

impl PassiveDevice for ConnectionS1X {}

impl Runnable for ConnectionS1X {
    type Confirm = Broadcast;
    type Report = ();

    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
        <Self as ConsecutivePassiveDevice>::run(self, rx_confirm, tx_report);
    }
}

impl ConsecutivePassiveDevice for ConnectionS1X {
    fn respond(&mut self) {
        self.in_s1_pre.ffw_accepted().into_iter().for_each(|s| self.out_s1_post.feedforward(self.refine(s)));
    }
    
    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.out_s1_post.running_passive_devices()
    }
}

impl Acceptor<FwdPreS1> for ConnectionS1X {
    fn add(&mut self, pre: WcMx<dyn Generator<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>) {
        self.in_s1_pre.add_target(pre, linker);
    }
}

impl Generator<FwdPostS1> for ConnectionS1X {
    fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<FwdPostS1>>, linker: AcMx<Linker<FwdPostS1>>) {
        self.out_s1_post.add_active_target(post, linker);
    }
    
    fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<FwdPostS1>>, linker: AcMx<Linker<FwdPostS1>>) {
        self.out_s1_post.add_passive_target(post, linker);
    }
}

impl ConnectionS1X {
    pub fn new(value: i32) -> AcMx<ConnectionS1X> {
        Arc::new(Mutex::new(ConnectionS1X {
            in_s1_pre: SingleInComponentS1Pre::new(),
            out_s1_post: SingleOutComponentS1Post::new(),
            value,
        }))
    }

    pub fn new_with_passive<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<ConnectionS1X>
    where G: 'static + Generator<FwdPreS1>,
          A: 'static + PassiveAcceptor<FwdPostS1>,
    {
        let conn = ConnectionS1X::new(value);
        connectivity::connect_passive(pre, conn.clone());
        connectivity::connect_passive(conn.clone(), post);
        conn
    }

    pub fn new_with_active<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<ConnectionS1X>
    where G: 'static + Generator<FwdPreS1>,
          A: 'static + ActiveAcceptor<FwdPostS1>,
    {
        let conn = ConnectionS1X::new(value);
        connectivity::connect_passive(pre, conn.clone());
        connectivity::connect_active(conn.clone(), post);
        conn
    }
    
    // pub fn new_with_passive(value: i32, pre: AcMx<dyn Generator<FwdPreS1>>>, post: AcMx<dyn PassiveAcceptor<FwdPostS1>>>) -> AcMx<ConnectionS1X>> {
    //     let conn = ConnectionS1X::new(value);
    //     connectivity::connect_passive(pre, conn.clone());
    //     connectivity::connect_passive(conn.clone(), post);
    //     conn
    // }

    // pub fn new_with_active(value: i32, pre: AcMx<dyn Generator<FwdPreS1>>>, post: AcMx<dyn ActiveAcceptor<FwdPostS1>>>) -> AcMx<ConnectionS1X>> {
    //     let conn = ConnectionS1X::new(value);
    //     connectivity::connect_passive(pre, conn.clone());
    //     connectivity::connect_active(conn.clone(), post);
    //     conn
    // }

    pub fn new_with_passive_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<ConnectionS1X>
    where PG: HoldDevices<G>,
          PA: HoldDevices<A>,
          G: 'static + Generator<FwdPreS1>,
          A: 'static + PassiveAcceptor<FwdPostS1>,
    {
        let device1 = p1.lock().unwrap().device_by_id(n1).clone();
        let device2 = p2.lock().unwrap().device_by_id(n2).clone();
        ConnectionS1X::new_with_passive(value, device1, device2)
    }

    pub fn new_with_active_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<ConnectionS1X>
    where PG: HoldDevices<G>,
          PA: HoldDevices<A>,
          G: 'static + Generator<FwdPreS1>,
          A: 'static + ActiveAcceptor<FwdPostS1>,
    {
        let device1 = p1.lock().unwrap().device_by_id(n1).clone();
        let device2 = p2.lock().unwrap().device_by_id(n2).clone();
        ConnectionS1X::new_with_active(value, device1, device2)
    }
    
    fn refine(&self, s: FwdPreS1) -> FwdPostS1 {
        FwdPostS1 {
            msg_gen: s.msg_gen,
            msg_prop: self.value,
        }
    }
}
