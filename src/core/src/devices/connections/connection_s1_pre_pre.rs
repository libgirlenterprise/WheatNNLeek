use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{AcMx, WcMx};
use crate::connectivity;
use crate::connectivity::{Generator, Acceptor, PassiveAcceptor, ActiveAcceptor};
use crate::connectivity::s1_pre::{SingleInComponentS1Pre, FwdPreS1, SingleOutComponentS1Pre};
use crate::operation::{Configurable, Runnable, RunningSet, Broadcast, PassiveDevice, RunMode};
use crate::operation::op_device::ConsecutivePassiveDevice;
use crate::populations::HoldDevices;
use crate::components::Linker;

pub struct ConnectionS1PrePre {
    in_s1_pre: SingleInComponentS1Pre,
    out_s1_pre: SingleOutComponentS1Pre,
    value: i32,
}

impl Configurable for ConnectionS1PrePre {
    fn config_mode(&mut self, mode: RunMode) {
        self.in_s1_pre.config_mode(mode);
        self.out_s1_pre.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.in_s1_pre.config_channels();
        self.out_s1_pre.config_channels();   
    }

    fn mode(&self) -> RunMode {
        RunMode::eq_mode(self.in_s1_pre.mode(),self.out_s1_pre.mode())
    }
}

impl PassiveDevice for ConnectionS1PrePre {}

impl Runnable for ConnectionS1PrePre {
    type Confirm = Broadcast;
    type Report = ();

    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
        <Self as ConsecutivePassiveDevice>::run(self, rx_confirm, tx_report);
    }
}

impl ConsecutivePassiveDevice for ConnectionS1PrePre {
    fn respond(&mut self) {
        self.value += 1;
        self.in_s1_pre.ffw_accepted().into_iter().for_each(|s| self.out_s1_pre.feedforward(self.refine(s)));
    }
    
    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.out_s1_pre.running_passive_devices()
    }
}

impl Acceptor<FwdPreS1> for ConnectionS1PrePre {
    fn add(&mut self, pre: WcMx<dyn Generator<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>) {
        self.in_s1_pre.add_target(pre, linker);
    }
}

impl Generator<FwdPreS1> for ConnectionS1PrePre {
    fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>) {
        self.out_s1_pre.add_active_target(post, linker);
    }
    
    fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>) {
        self.out_s1_pre.add_passive_target(post, linker);
    }
}

impl ConnectionS1PrePre {
    pub fn new(value: i32) -> AcMx<ConnectionS1PrePre> {
        Arc::new(Mutex::new(ConnectionS1PrePre {
            in_s1_pre: SingleInComponentS1Pre::new(),
            out_s1_pre: SingleOutComponentS1Pre::new(),
            value,
        }))
    }

    pub fn new_with_passive<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<ConnectionS1PrePre>
    where G: 'static + Generator<FwdPreS1>,
          A: 'static + PassiveAcceptor<FwdPreS1>,
    {
        let conn = ConnectionS1PrePre::new(value);
        connectivity::connect_passive(pre, conn.clone());
        connectivity::connect_passive(conn.clone(), post);
        conn
    }

    pub fn new_with_active<G, A>(value: i32, pre: AcMx<G>, post: AcMx<A>) -> AcMx<ConnectionS1PrePre>
    where G: 'static + Generator<FwdPreS1>,
          A: 'static + ActiveAcceptor<FwdPreS1>,
    {
        let conn = ConnectionS1PrePre::new(value);
        connectivity::connect_passive(pre, conn.clone());
        connectivity::connect_active(conn.clone(), post);
        conn
    }

    pub fn new_with_passive_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<ConnectionS1PrePre>
    where PG: HoldDevices<G>,
          PA: HoldDevices<A>,
          G: 'static + Generator<FwdPreS1>,
          A: 'static + PassiveAcceptor<FwdPreS1>,
    {
        let device1 = p1.lock().unwrap().device_by_id(n1).clone();
        let device2 = p2.lock().unwrap().device_by_id(n2).clone();
        ConnectionS1PrePre::new_with_passive(value, device1, device2)
    }

    pub fn new_with_active_population<G, A, PG, PA>(value: i32, p1: &AcMx<PG>, n1: usize, p2: &AcMx<PA>, n2: usize) -> AcMx<ConnectionS1PrePre>
    where PG: HoldDevices<G>,
          PA: HoldDevices<A>,
          G: 'static + Generator<FwdPreS1>,
          A: 'static + ActiveAcceptor<FwdPreS1>,
    {
        let device1 = p1.lock().unwrap().device_by_id(n1).clone();
        let device2 = p2.lock().unwrap().device_by_id(n2).clone();
        ConnectionS1PrePre::new_with_active(value, device1, device2)
    }
    
    fn refine(&self, s: FwdPreS1) -> FwdPreS1 {
        println!("ConnectionS1PrePre refine: get {}, self {}.", s.msg_gen, self.value);
        FwdPreS1 {
            msg_gen: s.msg_gen,
        }
    }
}
