# Network Ping Analysis

## 1. Ping to `kiribati.gov.ki` (85.187.128.46)
- **Location**: Kiribati (Pacific Ocean region)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 334.8ms / 415.3ms / 472.4ms
  - **Nodes Jumped Towards**: 27
  - **Nodes Jumped Back**: 17
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 415.3ms
  - 1024B: Avg RTT 428.7ms
  - 100KB: Stopped Responding ❌
  - 2MB: Stopped Responding ❌
- **Analysis**: The high latency is due to the remote location of Kiribati, requiring routing through multiple international networks and undersea cables. Large packet sizes cause failure, likely due to MTU limitations.

## 2. Ping to `cs.pwr.edu.pl` (156.17.7.22)
- **Location**: Poland (Wrocław)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 44.1ms / 76.3ms / 123.9ms
  - **Nodes Jumped Towards**: 5
  - **Nodes Jumped Back**: 2
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 76.3ms
  - 1024B: Avg RTT 79.5ms
  - 100KB: Avg RTT 85.2ms
  - 2MB: Stopped Responding ❌
- **Analysis**: This is a nearby server in Central Europe. The lower hop count suggests a direct routing path. Large packets increase RTT, with 2MB causing failure.

## 3. Ping to `olx.pl` (18.244.102.127)
- **Location**: Poland (Warsaw)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 36.2ms / 58.1ms / 78.1ms
  - **Nodes Jumped Towards**: 7
  - **Nodes Jumped Back**: 12
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 58.1ms
  - 1024B: Avg RTT 61.2ms
  - 100KB: Avg RTT 67.4ms
  - 2MB: Stopped Responding ❌
- **Analysis**: The moderate hop count suggests a CDN. RTT is stable, with 2MB packets failing.

## 4. Ping to `google.com` (142.250.179.142)
- **Location**: Likely a Google server in the Netherlands or the US
- **Ping Results**:
  - **Min/Avg/Max RTT**: 93.5ms / 136.4ms / 235ms
  - **Nodes Jumped Towards**: 6
  - **Nodes Jumped Back**: 18
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 136.4ms
  - 1024B: Avg RTT 140.9ms
  - 100KB: Avg RTT 149.2ms
  - 2MB: Avg RTT 180.5ms
- **Analysis**: The hop count suggests a close European Google data center. Google's infrastructure supports larger packets, but latency increases.

## 5. Ping to `id.wikipedia.org` (185.15.59.224)
- **Location**: Wikimedia servers (likely in Europe or the US)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 79.8ms / 100.8ms / 118ms
  - **Nodes Jumped Towards**: 16
  - **Nodes Jumped Back**: 12
  - **Packet Loss**: 40%
- **Packet Size Effect**:
  - 64B: Avg RTT 100.8ms
  - 1024B: Avg RTT 104.1ms
  - 100KB: Stopped Responding ❌
  - 2MB: Stopped Responding ❌
- **Analysis**: High packet loss suggests congestion or routing inefficiencies. Larger packets fail, possibly due to firewall or network restrictions.

## 6. Ping to `tierradelfuego.org.ar` (200.0.230.196)
- **Location**: Argentina (Southernmost region)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 352.4ms / 395.5ms / 533.3ms
  - **Nodes Jumped Towards**: 22
  - **Nodes Jumped Back**: 22
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 395.5ms
  - 1024B: Avg RTT 405.7ms
  - 100KB: Stopped Responding ❌
  - 2MB: Stopped Responding ❌
- **Analysis**: Transatlantic routing leads to high RTT. Large packets cause failures, likely due to bandwidth constraints.

## 7. Ping to `cocosseaview.com` (202.164.22.100)
- **Location**: Hotel in the Indian Ocean
- **Ping Results**:
  - **Min/Avg/Max RTT**: 392ms / 510ms / 585ms
  - **Nodes Jumped Towards**: 22
  - **Nodes Jumped Back**: 22
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 510ms
  - 1024B: Avg RTT 523.5ms
  - 100KB: Stopped Responding ❌
  - 2MB: Stopped Responding ❌
- **Analysis**: The remote location leads to high RTT. Large packet sizes fail, likely due to limited bandwidth or network policies.
