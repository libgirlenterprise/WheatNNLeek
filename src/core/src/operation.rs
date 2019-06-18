extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::thread;
use std::thread::JoinHandle;
use crate::{AcMx, Time};
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
    fn mode(&self) -> RunMode;
}

// for supervisor / populations.
pub trait Active: Configurable {
    type Report: Send;
    fn run(&mut self, dt: Time, time: Time);
    fn confirm_sender(&self) -> CCSender<Broadcast>;
    fn confirm_receiver(&self) -> CCReceiver<Broadcast>;
    fn report_receiver(&self) -> CCReceiver<<Self as Active>::Report>;
    fn report_sender(&self) -> CCSender<<Self as Active>::Report>;
}

// for connectivity; distinguish between PassiveAgent only.
pub trait ActiveAgent: Agent {}

// for supervisor / populations.
pub trait Passive: Configurable {
    fn run(&mut self);
    fn confirm_sender(&self) -> CCSender<Broadcast>;
    fn confirm_receiver(&self) -> CCReceiver<Broadcast>;
}

// for PassivePopulation & connectivity / OutComponents
pub trait PassiveAgent: Passive + Agent {
    fn recheck_mode(&mut self);
    fn report_sender(&self) -> CCSender<()>;
    fn passive_back_ope_chs(&self) -> PassiveBackOpeChs;
}

pub struct PassiveRunningSet {
    pub instance: JoinHandle<()>,
    pub confirm: CCSender<Broadcast>,
}

impl PassiveRunningSet {
    pub fn new<T>(agent: AcMx<T>) -> Option<PassiveRunningSet>
    where T: 'static + Passive + Send + ?Sized
    {
        let m = agent.lock().unwrap().mode();
        match m {
            RunMode::Idle => None,
            RunMode::ForwardStepping => {
                let confirm = agent.lock().unwrap().confirm_sender();
                Some(PassiveRunningSet {
                    instance: thread::spawn(move || {agent.lock().unwrap().run()}),
                    confirm,
                })
            },
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

pub struct ActiveRunningSet<R> {
    pub instance: JoinHandle<()>,
    pub confirm: CCSender<Broadcast>,
    pub report: CCReceiver<R>,
}

impl<R> ActiveRunningSet<R> {
    pub fn new<T>(agent: AcMx<T>) -> Option<ActiveRunningSet<<T as Active>::Report>>
    where T: 'static + Active + Send + ?Sized
    {
        let m = agent.lock().unwrap().mode();
        match m {
            RunMode::Idle => None,
            RunMode::ForwardStepping => {
                let confirm = agent.lock().unwrap().confirm_sender();
                let report = agent.lock().unwrap().report_receiver();            
                Some(ActiveRunningSet {
                    instance: thread::spawn(move || {agent.lock().unwrap().run()}),
                    confirm,
                    report,
                })
            },
            RunMode::ForwardRealTime => panic!("ForwardRealTime not yet implemented!"),
        }
    }
}

pub struct PassivePopulationOpeChs {
    confirm_sender: CCSender<Broadcast>,
    confirm_receiver: CCReceiver<Broadcast>,
}

impl PassivePopulationOpeChs {
    pub fn new() -> PassivePopulationOpeChs {
        let (tx, rx) = crossbeam_channel::bounded(1);
        PassivePopulationOpeChs {
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

pub struct OpeChs<R: Send> {
    confirm_sender: CCSender<Broadcast>,
    confirm_receiver: CCReceiver<Broadcast>,
    report_receiver: CCReceiver<R>,
    report_sender: CCSender<R>,
}

impl<R:Send> OpeChs<R> {
    pub fn new() -> OpeChs<R> {
        let (c_tx, c_rx) = crossbeam_channel::bounded(1);
        let (r_tx, r_rx) = crossbeam_channel::bounded(1);
        OpeChs {
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

impl OpeChs<()> {
    pub fn passive_back_ope_chs(&self) -> PassiveBackOpeChs {
        PassiveBackOpeChs {
            confirm_sender: self.confirm_sender.clone(),
            report_receiver: self.report_receiver.clone(),
        }
    }    
}

pub struct PassiveBackOpeChs {
    confirm_sender: CCSender<Broadcast>,
    report_receiver: CCReceiver<()>,
}

impl PassiveBackOpeChs {
    fn send_confirm(&self, s: Broadcast) {
        self.confirm_sender.send(s).unwrap()
    }

    fn recv_report(&self) -> () {
        self.report_receiver.recv().unwrap()
    }
}
