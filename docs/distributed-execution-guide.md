<!-- verified-against: 0.99.46 -->
# Distributed Execution Guide

This guide covers q's **opt-in** distributed execution feature (verified against v0.99.46, originally introduced for MAS Schritt 6 Phase 2). High-risk tool execution can be routed to a remote executor node over mTLS-secured TCP, isolating hostile code from the developer machine.

> **Default is local-only.** No configuration changes are needed for local development. The broker is strictly opt-in behind `mas.broker.enabled = false`.

## Table of Contents

- [Quick Start: Local Dev (No Broker)](#quick-start-local-dev-no-broker)
- [Remote Executor Setup](#remote-executor-setup)
- [Cloud VM Deployment](#cloud-vm-deployment)
- [Configuration Reference](#configuration-reference)
- [Troubleshooting](#troubleshooting)

---

## Quick Start: Local Dev (No Broker)

No action needed. The default configuration keeps all execution local:

```yaml
# ~/.q/config.yml — default (everything local)
mas:
  broker:
    enabled: false
```

All tool requests execute via the local stdio subprocess path. No TCP listeners are started. No certificates are needed.

---

## Remote Executor Setup

This 5-step guide gets a remote executor running. You need:
- Two machines (or two terminals on localhost for testing): an **orchestrator** (developer machine) and an **executor** (remote node).
- OpenSSL 3.x or later on both.
- Racket 8.10+ on both.

### Step 1: Generate Certificates (on orchestrator)

```bash
q generate-certificates --output-dir ~/.q/certs
```

This creates a local CA and mutual TLS certificates:

```
~/.q/certs/
  ca-cert.pem        # Root CA certificate
  ca-key.pem         # Root CA private key (KEEP SECRET)
  server-cert.pem    # Executor node certificate
  server-key.pem     # Executor node private key
  client-cert.pem    # Orchestrator certificate
  client-key.pem     # Orchestrator private key
```

> **Deploy the right certs to each machine:**
> - Executor node: `ca-cert.pem`, `server-cert.pem`, `server-key.pem`
> - Orchestrator: `ca-cert.pem`, `client-cert.pem`, `client-key.pem`
> - Never share `ca-key.pem` after initial setup.

### Step 2: Deploy the Executor Node

On the executor machine, copy the server certs and run:

```bash
# Create cert directory
mkdir -p ~/.q/certs
# Copy ca-cert.pem, server-cert.pem, server-key.pem into ~/.q/certs/

# Start the executor server
racket -t sandbox/executor-server.rkt
```

The executor listens on port **8443** by default. It requires:
- A valid client certificate (mTLS) for every connection.
- A valid capability token (HMAC-SHA256) for every request.

### Step 3: Configure the Orchestrator

On the developer machine, copy the client certs and enable the broker:

```yaml
# ~/.q/config.yml
mas:
  broker:
    enabled: true
    remote-host: "executor.example.com"
    remote-port: 8443
    cert-dir: "~/.q/certs/"
    capability-secret: "your-shared-secret-here"
```

> **The `capability-secret` must be set when the broker is enabled.** If it's missing, q will fail fast with an error. This is a deliberate fail-closed design.

### Step 4: Verify Connectivity

```bash
# Verify the executor is reachable and TLS handshake succeeds
q generate-certificates --force  # regenerate if needed
# Then start q — it will attempt to connect on first high-risk tool request
```

If the connection succeeds, high-risk tool requests will be routed to the remote executor. Low and medium risk requests still execute locally for speed.

### Step 5: Go!

Start q as normal. The tool gateway will now use risk-based routing:
- **Low risk** → local execution (fast)
- **Medium risk** → local execution
- **High/critical risk** → remote executor (isolated)

The circuit breaker will protect against executor outages: if the remote executor fails 5 consecutive times, the circuit opens and fast-fails requests for 30 seconds before attempting reconnection.

---

## Cloud VM Deployment

### Hetzner Cloud

```bash
# Create a CX21 instance
hcloud server create --name q-executor --type cx21 --image ubuntu-24.04

# SSH in and install Racket
ssh root@<executor-ip>
apt update && apt install -y racket
# Copy server certs and start the executor
```

### AWS EC2

```bash
# Launch a t3.medium instance
aws ec2 run-instances --image-id ami-xxxxx --instance-type t3.medium ...

# Security group: allow inbound TCP 8443 from your orchestrator IP only
aws ec2 authorize-security-group-ingress \
  --group-id sg-xxxxx \
  --protocol tcp --port 8443 \
  --cidr <orchestrator-ip>/32
```

### GCP Compute Engine

```bash
# Create a VM
gcloud compute instances create q-executor \
  --machine-type=e2-medium \
  --image-family=ubuntu-2404-lts

# Firewall rule: allow 8443 from orchestrator IP
gcloud compute firewall-rules create allow-q-executor \
  --allow tcp:8443 \
  --source-ranges=<orchestrator-ip>/32
```

> **Security best practice:** Restrict inbound port 8443 to your orchestrator IP only. Never expose the executor to the public internet without mTLS.

---

## Configuration Reference

All settings are under the `mas.broker.*` namespace:

| Key | Type | Default | Description |
|-----|------|---------|-------------|
| `mas.broker.enabled` | boolean | `false` | Master gate. If false, no TCP listeners are started and all execution is local. |
| `mas.broker.remote-host` | string | `"localhost"` | Hostname/IP of the remote executor node. |
| `mas.broker.remote-port` | integer | `8443` | TCP port of the remote executor node. |
| `mas.broker.cert-dir` | string | `"~/.q/certs/"` | Directory containing CA, client cert, and client key PEM files. |
| `mas.broker.capability-secret` | string | `#f` | HMAC-SHA256 secret for capability token signing. **Required when enabled.** |

### Certificate File Naming

The `cert-dir` must contain these files (generated by `q generate-certificates`):

| File | Purpose |
|------|---------|
| `ca-cert.pem` | CA certificate for verifying the remote server's identity |
| `client-cert.pem` | Client certificate (presented to the executor for mTLS) |
| `client-key.pem` | Client private key (paired with client-cert) |

The executor node additionally needs:

| File | Purpose |
|------|---------|
| `server-cert.pem` | Server certificate (presented to orchestrator for mTLS) |
| `server-key.pem` | Server private key |

---

## Troubleshooting

### Certificate Errors

**"certificate verify failed"**
- The CA certificate doesn't match. Ensure the same `ca-cert.pem` is on both machines.
- Check that `cert-dir` points to the correct directory.

**"no protocols available"**
- OpenSSL 3.x may disable older TLS versions. q uses TLS 1.2 exclusively. Ensure your OpenSSL supports TLS 1.2 (it should on any modern system).

**"key values mismatch"**
- The cert and key files don't pair. Regenerate with `q generate-certificates --force`.

### Connection Refused

**"connection refused"**
- The executor isn't running or is on a different port. Check `remote-port` matches the executor's listen port.
- Firewall blocking the port. Verify the security group/firewall allows TCP 8443.

**"connection timeout"**
- Network latency or firewall. Increase the connect timeout or check network routing.

### Circuit Breaker

**"circuit breaker open"**
- The remote executor has failed 5 consecutive times. The circuit is now open and fast-failing requests.
- **Fix:** Check the executor is running and reachable. The circuit will auto-recover after 30 seconds (half-open state), then close on successful request.

**"circuit breaker half-open → re-opened"**
- The trial request after cooldown also failed. The executor is still down. Fix the underlying issue.

### Capability Token Errors

**"invalid capability token"**
- The `capability-secret` doesn't match between orchestrator and executor.
- Ensure the same secret string is configured on both sides.

**"token expired"**
- Capability tokens have a 5-minute TTL. This normally isn't an issue since tokens are generated per-request. If you see this, check clock skew between machines.

### Debug Logging

Enable trace logging to see broker routing decisions:

```yaml
agent:
  trace-enabled: true
```

This will log routing decisions, connection state changes, and circuit breaker transitions to the trace log.
