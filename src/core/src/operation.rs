use std::sync::{Arc, Mutex};
extern crate crossbeam_channel;
use crossbeam_channel::Receiver as CCReceiver;
use crossbeam_channel::Sender as CCSender;
use std::thread;
use std::thread::JoinHandle;

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
    fn mode(&self) -> RunMode;
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
