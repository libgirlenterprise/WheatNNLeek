use std::sync::{Arc, Mutex};
extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::thread;
use std::thread::JoinHandle;
use crate::{AcMx};

pub mod op_population;
pub mod op_agent;

pub enum Broadcast {
    Evolve,
    Respond,
    Exit,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RunMode {
    Idle,
    ForwardStepping,
    ForwardRealTime,
}

impl RunMode {
    pub fn eq_mode(m1: RunMode, m2: RunMode) -> Result<RunMode, (RunMode, RunMode)> {
        if m1 == m2 {
            Ok(m1)
        } else {
            Err((m1, m2))
        }
    }
}

pub enum AgentRunMode<FS> {
    Idle,
    ForwardStepping(FS),
    ForwardRealTime, // not implemented
}

impl<F> AgentRunMode<F> {
    pub fn variant(&self) -> RunMode {
        match &self {
            AgentRunMode::Idle => RunMode::Idle,
            AgentRunMode::ForwardStepping(_) => RunMode::ForwardStepping,
            AgentRunMode::ForwardRealTime => RunMode::ForwardRealTime,
        }
    }

    pub fn eq_mode<F1, F2>(m1: AgentRunMode<F1>, m2: AgentRunMode<F2>) -> RunMode {
        match (m1, m2) {
            (AgentRunMode::Idle, AgentRunMode::Idle) => RunMode::Idle,
            (AgentRunMode::ForwardStepping(_), AgentRunMode::ForwardStepping(_)) => RunMode::ForwardStepping,
            _ => panic!("Runmode mismatch at check!"),
        }
    }
}

pub enum Fired {
    Y,
    N,
}

pub trait Configurable {
    fn config_mode(&mut self, mode: RunMode);
    fn config_channels(&mut self);
    // fn mode(&self) -> RunMode;
}

/// for RunningSet::new()
pub trait Runnable {
    type Confirm: Send;
    type Report: Send;
    fn run(&mut self, rx_confirm: CCReceiver<<Self as Runnable>::Confirm>, tx_report: CCSender<<Self as Runnable>::Report>);
}

/// for connectivity
pub trait ActiveAgent: Configurable {}

/// for PassivePopulation & connectivity / OutComponents
pub trait PassiveAgent: Runnable<Confirm = Broadcast, Report = ()> + Configurable {}

pub trait Active {
    // type Confirm: Send;
    type Report: Send;
    fn run(&mut self);
    fn confirm_sender(&self) -> CCSender<Broadcast>;
    fn confirm_receiver(&self) -> CCReceiver<Broadcast>;
    fn report_receiver(&self) -> CCReceiver<<Self as Active>::Report>;
    fn report_sender(&self) -> CCSender<<Self as Active>::Report>;
}

pub trait Passive {
    fn run(&mut self);
    fn confirm_sender(&self) -> CCSender<Broadcast>;
    fn confirm_receiver(&self) -> CCReceiver<Broadcast>;
    
    // fn make_fore_ch_pair(&mut self) -> (CCSender<Broadcast>, CCReceiver<()>);
    // fn fore_ch_pairs(&mut self) -> Vec<(CCReceiver<Broadcast>, CCSender<()>)>;
    
}

pub struct RunningSet<C: Send, R: Send> {
    pub instance: JoinHandle<()>,
    pub confirm: CCSender<C>,
    pub report: CCReceiver<R>,
}

impl<C: Send, R: Send> RunningSet<C, R> {
    pub fn new<T>(agent: Arc<Mutex<T>>) -> RunningSet<<T as Runnable>::Confirm, <T as Runnable>::Report>
    where T: 'static + Runnable + Send + ?Sized
    {
        // for strict ordering of agent-connection_prop, bounded(1) is chosen.
        let (tx_confirm, rx_confirm) = crossbeam_channel::bounded(1);
        let (tx_report, rx_report) = crossbeam_channel::bounded(1);
        RunningSet {
            instance: thread::spawn(move || {agent.lock().unwrap().run(rx_confirm, tx_report)}),
            confirm: tx_confirm,
            report: rx_report,
        }
    }
}

// impl<C: Send, R: Send> RunningSet<C, R> {
//     pub fn new<T>(agent: Arc<Mutex<T>>) -> RunningSet<<T as Runnable>::Confirm, <T as Runnable>::Report>
//     where T: 'static + Runnable + Send + ?Sized
//     {
//         // for strict ordering of agent-connection_prop, bounded(1) is chosen.
//         let unwrapped_agent = agent.lock().unwrap();
//         print!("Agent locked within RunningSet::new().");
//         RunningSet {
//             confirm: unwrapped_agent.confirm_sender(),
//             report: unwrapped_agent.report_receiver(),
//             // instance: thread::spawn(move || unwrapped_agent.run()),
//             instance: thread::spawn(move || {agent.lock().unwrap().run()}),
//         }
//     }
// }

pub struct PassiveRunningSet {
    pub instance: JoinHandle<()>,
    pub confirm: CCSender<Broadcast>,
}

impl PassiveRunningSet {
    pub fn new<T>(agent: AcMx<T>) -> PassiveRunningSet
    where T: 'static + Passive + Send + ?Sized
    {
        let confirm = agent.lock().unwrap().confirm_sender();
        PassiveRunningSet {
            instance: thread::spawn(move || {agent.lock().unwrap().run()}),
            confirm,
        }
    }
}

pub struct ActiveRunningSet<R> {
    pub instance: JoinHandle<()>,
    pub confirm: CCSender<Broadcast>,
    pub report: CCReceiver<R>,
}

impl<R> ActiveRunningSet<R> {
    pub fn new<T>(agent: AcMx<T>) -> ActiveRunningSet<<T as Active>::Report>
    where T: 'static + Active + Send + ?Sized
    {
        let mut confirm;
        let mut report;
        {
            let unwraped_agent = agent.lock().unwrap();
            confirm = unwraped_agent.confirm_sender();
            report = unwraped_agent.report_receiver();            
        }
        ActiveRunningSet {
            confirm,
            report,
            instance: thread::spawn(move || {agent.lock().unwrap().run()}),
        }
        
        // let unwrapped_agent = agent.lock().unwrap();
        // ActiveRunningSet {
        //     confirm: unwrapped_agent.confirm_sender(),
        //     report: unwrapped_agent.report_receiver(),
        //     instance: thread::spawn(move || {agent.lock().unwrap().run()}),
        // }
    }
}
