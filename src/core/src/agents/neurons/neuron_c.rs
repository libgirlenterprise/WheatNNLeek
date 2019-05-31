/// multi-in S1Post, multi-out S1Pre

use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::sync::{Mutex, Arc};
use crate::{WcMx, AcMx};
use crate::connectivity::s1_pre::{MultiOutComponentS1Pre, FwdPreS1};
use crate::connectivity::s1_post::{MultiInComponentS1Post, FwdPostS1};
use crate::connectivity::{Generator, Acceptor, ActiveAcceptor, PassiveAcceptor};
use crate::operation::{ActiveDevice, Configurable, Runnable, Broadcast, Fired, RunMode, RunningSet};
use crate::operation::op_device::FiringActiveDevice;
use crate::components::Linker;

pub struct NeuronC {
    out_s1_pre: MultiOutComponentS1Pre,
    in_s1_post: MultiInComponentS1Post,
    gen_value: i32,
    proc_value: i32,
    event_cond: Option<i32>,
    stock: Vec<FwdEndProduct>,
}

struct FwdEndProduct {
    pub msg_gen: i32,
    pub msg_prop: i32,
    pub msg_proc: i32,
}

impl Generator<FwdPreS1> for NeuronC {
    fn add_active(&mut self, post: WcMx<dyn ActiveAcceptor<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>)
    {
        self.out_s1_pre.add_active_target(post, linker);
    }

    fn add_passive(&mut self, post: WcMx<dyn PassiveAcceptor<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>)
    {
        self.out_s1_pre.add_passive_target(post, linker);
    }
}

impl Acceptor<FwdPostS1> for NeuronC {
    fn add(&mut self, pre: WcMx<dyn Generator<FwdPostS1>>, linker: AcMx<Linker<FwdPostS1>>)
    {
        self.in_s1_post.add_target(pre, linker);
    }
}

impl Configurable for NeuronC {
    fn config_mode(&mut self, mode: RunMode) {
        self.in_s1_post.config_mode(mode);
        self.out_s1_pre.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        self.in_s1_post.config_channels();
        self.out_s1_pre.config_channels();   
    }

    fn mode(&self) -> RunMode {
        RunMode::eq_mode(self.in_s1_post.mode(),self.out_s1_pre.mode())
    }
}

impl ActiveDevice for NeuronC {}

impl Runnable for NeuronC {
    type Confirm = Broadcast;
    type Report = Fired;
    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
        <Self as FiringActiveDevice>::run(self, rx_confirm, tx_report);
    }
}

impl FiringActiveDevice for NeuronC {
    fn end(&mut self) {
        self.accept();
    }
    
    fn evolve(&mut self) -> Fired {
        self.proc_value += 1;
        self.gen_value += 1;
        self.accept();
        match self.event_cond {
            None => {
                // println!("agnet a go on. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
                Fired::N   
            },
            Some(n) => {
                match self.proc_value % n {
                    0 => {
                        println!("agnet c fire. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
                        self.generate();
                        Fired::Y
                    },
                    _ => {
                        // println!("agnet a go on. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
                        Fired::N
                    },
                }
            }
        }
    }

    fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>> {
        self.out_s1_pre.running_passive_devices()
    }
}

impl NeuronC {
    pub fn new(gen_value: i32, proc_value: i32, event_cond: Option<i32>) -> AcMx<NeuronC> {
        Arc::new(Mutex::new(
            NeuronC {
                out_s1_pre: MultiOutComponentS1Pre::new(),
                in_s1_post: MultiInComponentS1Post::new(),
                gen_value,
                proc_value,
                event_cond,
                stock: Vec::new(),
            }
        ))
    }
    
    fn generate(&self) {
        self.out_s1_pre.feedforward(FwdPreS1 {
            msg_gen: self.gen_value,
        });
    }

    fn accept(&mut self) {
        let mut acc = self.in_s1_post.ffw_accepted().iter().map(|s| FwdEndProduct {
                msg_gen: s.msg_gen,
                msg_prop: s.msg_prop,
                msg_proc: self.proc_value,
            }  
        ).collect::<Vec<FwdEndProduct>>();

        // for demo accepting
        for msg in &acc {
            println!(
                "agent c accept: gen: {}, prop: {}, proc: {}.",
                msg.msg_gen,
                msg.msg_prop,
                msg.msg_proc
            )
        }
        
        self.stock.append(&mut acc);
    }

    // pub fn print_values(&self) {
    //     println!("gen: {}, proc: {}.", self.gen_value, self.proc_value);
    // }
    
    pub fn show(&self) {
        for msg in &self.stock {
            println!(
                "agent c buffer: gen: {}, prop: {}, proc: {}.",
                msg.msg_gen,
                msg.msg_prop,
                msg.msg_proc
            )
        }
    }
}
