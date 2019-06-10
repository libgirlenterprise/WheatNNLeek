use std::sync::{Arc, Mutex};
extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::thread;
use std::thread::JoinHandle;
use crate::{AcMx};
use crate::agents::{Agent};

pub mod op_population;
pub mod op_agent;

#[derive(Debug)]
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
// pub trait PassiveAgent: Runnable<Confirm = Broadcast, Report = ()> + Configurable {}
pub trait PassiveAgent: Passive + Agent {
    // fn report_receiver(&self) -> CCReceiver<()>;
    fn report_sender(&self) -> CCSender<()>;
    fn passive_sync_chs_set(&self) -> PassiveSyncChsSet;
}

pub trait Active: Configurable {
    // type Confirm: Send;
    type Report: Send;
    fn run(&mut self);
    fn confirm_sender(&self) -> CCSender<Broadcast>;
    fn report_receiver(&self) -> CCReceiver<<Self as Active>::Report>;
}

pub trait Passive: Configurable {
    fn run(&mut self);
    fn confirm_sender(&self) -> CCSender<Broadcast>;
    fn confirm_receiver(&self) -> CCReceiver<Broadcast>;    
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

pub struct OpeChsGenC {
    confirm_sender: CCSender<Broadcast>,
    confirm_receiver: CCReceiver<Broadcast>,
}

impl OpeChsGenC {
    pub fn new() -> OpeChsGenC {
        let (tx, rx) = crossbeam_channel::bounded(1);
        OpeChsGenC {
            confirm_sender: tx,
            confirm_receiver: rx,
        }       
    }

    pub fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.confirm_sender.clone()
    }

    pub fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.confirm_receiver.clone()
    }
}

pub struct OpeChsGenCR<R: Send> {
    confirm_sender: CCSender<Broadcast>,
    confirm_receiver: CCReceiver<Broadcast>,
    report_receiver: CCReceiver<R>,
    report_sender: CCSender<R>,
}

impl<R:Send> OpeChsGenCR<R> {
    pub fn new() -> OpeChsGenCR<R> {
        let (c_tx, c_rx) = crossbeam_channel::bounded(1);
        let (r_tx, r_rx) = crossbeam_channel::bounded(1);
        OpeChsGenCR {
            confirm_sender: c_tx,
            confirm_receiver: c_rx,
            report_receiver: r_rx,
            report_sender: r_tx,
        }       
    }

    pub fn confirm_sender(&self) -> CCSender<Broadcast> {
        self.confirm_sender.clone()
    }

    pub fn confirm_receiver(&self) -> CCReceiver<Broadcast> {
        self.confirm_receiver.clone()
    }

    pub fn report_receiver(&self) -> CCReceiver<R> {
        self.report_receiver.clone()
    }

    pub fn report_sender(&self) -> CCSender<R> {
        self.report_sender.clone()
    }
}

pub struct PassiveSyncChsSet {
    confirm: CCSender<Broadcast>,
    report: CCReceiver<()>,
}

impl PassiveSyncChsSet {
    fn send_confirm(&self, s: Broadcast) {
        self.confirm.send(s).unwrap()
    }

    fn recv_report(&self) -> () {
        self.report.recv().unwrap()
    }
}
