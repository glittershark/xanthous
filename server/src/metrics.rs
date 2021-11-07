pub use ::metrics::*;

pub mod reported {
    /// Counter: Connections accepted on the TCP listener
    pub const CONNECTIONS_ACCEPTED: &str = "ssh.connections.accepted";

    /// Histogram: Connection duration
    pub const CONNECTION_DURATION: &str = "ssh.connections.duration";

    /// Gauge: Currently active connections
    pub const ACTIVE_CONNECTIONS: &str = "ssh.connections.active";

    /// Gauge: Currently running xanthous processes
    pub const RUNNING_PROCESSES: &str = "ssh.child.processes";
}

pub fn register() {
    use reported::*;

    register_counter!(CONNECTIONS_ACCEPTED);
    register_histogram!(CONNECTION_DURATION);
    register_gauge!(ACTIVE_CONNECTIONS);
    register_gauge!(RUNNING_PROCESSES);
}
