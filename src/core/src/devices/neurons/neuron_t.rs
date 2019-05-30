/// multi-in/out S1Pre.

// use crossbeam_channel::Receiver as CCReceiver;
// use crossbeam_channel::Sender as CCSender;
// use std::sync::{Mutex, Arc};
// use crate::{WkMx, AcMx};
use crate::signals::s1_pre::{FwdPreS1, NeuronPreSynComponentS1};
// use crate::connectivity::s1_post::{FwdPostS1, NeuronPostSynComponentS1};
// use crate::connectivity::{Generator, Acceptor, ActiveAcceptor, PassiveAcceptor};
use crate::operation::{ActiveDevice, Configurable, Runnable, Broadcast, Fired, RunMode, RunningSet};
// use crate::operation::op_device::FiringActiveDevice;
// use crate::components::Linker;

pub struct NeuronT {
    out_s1_pre: NeuronPreSynComponentS1,
    // in_s1_post: MultiInComponentS1Post,
    // gen_value: i32,
    // proc_value: i32,
    // event_cond: Option<i32>,
    // stock: Vec<FwdEndProduct>,
}

// struct FwdEndProduct {
//     pub msg: i32,
//     pub proc: i32,
// }

// impl Generator<FwdPreS1> for NeuronT {
//     fn add_active(&mut self, post: WkMx<dyn ActiveAcceptor<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>)
//     {
//         self.out_s1_pre.add_active_target(post, linker);
//     }

//     fn add_passive(&mut self, post: WkMx<dyn PassiveAcceptor<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>)
//     {
//         self.out_s1_pre.add_passive_target(post, linker);
//     }
// }

// impl Acceptor<FwdPreS1> for NeuronT {
//     fn add(&mut self, pre: WkMx<dyn Generator<FwdPreS1>>, linker: AcMx<Linker<FwdPreS1>>)
//     {
//         self.in_s1_pre.add_target(pre, linker);
//     }
// }

impl Configurable for NeuronT {
    fn config_mode(&mut self, mode: RunMode) {
        // self.in_s1_pre.config_mode(mode);
        self.out_s1_pre.config_mode(mode);
    }
    
    fn config_channels(&mut self) {
        // self.in_s1_pre.config_channels();
        self.out_s1_pre.config_channels();   
    }

    fn mode(&self) -> RunMode {
        panic!("NeuronT COnfigurable not yet done!");
        // RunMode::eq_mode(self.in_s1_pre.mode(),self.out_s1_pre.mode())
    }
}

impl ActiveDevice for NeuronT {}

// impl Runnable for NeuronT {
//     type Confirm = Broadcast;
//     type Report = Fired;
//     fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>) {
//         <Self as FiringActiveDevice>::run(self, rx_confirm, tx_report);
//     }
// }

// impl FiringActiveDevice for NeuronT {
//     fn end(&mut self) {
//         self.accept();
//     }
    
//     fn evolve(&mut self) -> Fired {
//         self.proc_value += 1;
//         self.gen_value += 1;
//         self.accept();
//         match self.event_cond {
//             None => {
//                 // println!("agnet a go on. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
//                 Fired::N   
//             },
//             Some(n) => {
//                 match self.proc_value % n {
//                     0 => {
//                         println!("agnet c fire. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
//                         self.generate();
//                         Fired::Y
//                     },
//                     _ => {
//                         // println!("agnet a go on. gen: {}, proc: {}.",  self.gen_value, self.proc_value);
//                         Fired::N
//                     },
//                 }
//             }
//         }
//     }

//     fn running_passive_devices(&self) -> Vec<RunningSet<Broadcast, ()>> {
//         self.out_s1_pre.running_passive_devices()
//     }
// }

// impl NeuronT {
//     pub fn new(gen_value: i32, proc_value: i32, event_cond: Option<i32>) -> AcMx<NeuronT> {
//         Arc::new(Mutex::new(
//             NeuronT {
//                 out_s1_pre: MultiOutComponentS1Pre::new(),
//                 in_s1_pre: MultiInComponentS1Pre::new(),
//                 gen_value,
//                 proc_value,
//                 event_cond,
//                 stock: Vec::new(),
//             }
//         ))
//     }
    
//     fn generate(&self) {
//         self.out_s1_pre.feedforward(FwdPreS1 {
//             msg_gen: self.gen_value,
//         });
//     }

//     fn accept(&mut self) {
//         let mut acc = self.in_s1_pre.ffw_accepted().iter().map(|s| FwdEndProduct {
//             msg: s.msg_gen,
//             proc: self.proc_value,
//         }).collect::<Vec<FwdEndProduct>>();

//         // for demo accepting
//         for msg in &acc {
//             println!(
//                 "agent c accept: gen: {}, proc: {}.",
//                 msg.msg,
//                 msg.proc
//             )
//         }
        
//         self.stock.append(&mut acc);
//     }

//     // pub fn print_values(&self) {
//     //     println!("gen: {}, proc: {}.", self.gen_value, self.proc_value);
//     // }
    
//     pub fn show(&self) {
//         for msg in &self.stock {
//             println!(
//                 "agent c buffer: gen: {}, proc: {}.",
//                 msg.msg,
//                 msg.proc
//             )
//         }
//     }
// }
