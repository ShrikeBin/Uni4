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
  - **Min/Avg/Max RTT**: 5.15ms / 6.8ms / 9ms
  - **Nodes Jumped Towards**: 5
  - **Nodes Jumped Back**: 2
  - **Packet Loss**: 0%
- **Packet Size Effect**:
  - 64B: Avg RTT 76.3ms
  - 1024B: Avg RTT 79.5ms
  - 1500B: Avg RTT 85.2ms
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
  - 1500B: Avg RTT 67.4ms
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


## 8. Table:

| Service                     | Packet Size (Bytes) | Jumps | Jumps Back | Avg RTT (ms) | Avg RTT per Jump (ms) |
|:---------------------------:|----------:|-----:|----------:|------------:|---------------------:|
| kiribati.gov.ki             | 64 &nbsp;&emsp;&emsp;&emsp;          |  27   |     17     |     415.3    |         15.4         |
| (very far away)             | 1024 &nbsp;&emsp;&emsp;&emsp;      |  27   |     17     |     428.7    |         15.9         |
|                             | 1500&nbsp;&emsp;&emsp;&emsp;       |  25   |     16     |     481.3    |         17.8         |
| cs.pwr.edu.pl               | 64   &nbsp;&emsp;&emsp;&emsp;      |   5   |     2      |      76.3    |         15.3         |
| (very close)                | 1024  &nbsp;&emsp;&emsp;&emsp;     |   5   |     2      |      2.1     |         0.3          |
|                             | 1500 &nbsp;&emsp;&emsp;&emsp;      |   5   |     2      |      3.5     |         0.5          |
| olx.pl                      | 64  &nbsp;&emsp;&emsp;&emsp;       |   7   |     12     |      58.1    |         8.3          |
| (close)                     | 1024 &nbsp;&emsp;&emsp;&emsp;      |   7   |     12     |      61.2    |         8.7          |
|                             | 1500 &nbsp;&emsp;&emsp;&emsp;      |   ❌   |     ❌     |   ❌ |        N/A         |
| google.com                  | 64 &nbsp;&emsp;&emsp;&emsp;        |   6   |     18     |     136.4    |         22.7         |
| (big service, relatively close) | 1024&nbsp;&emsp;&emsp;&emsp;   |   6   |     18     |     140.9    |         23.5         |
|                             | 1500&nbsp;&emsp;&emsp;&emsp;       |   ❌   |     ❌     |  ❌  |        N/A         |
| id.wikipedia.org            | 64  &nbsp;&emsp;&emsp;&emsp;       |  16   |     12     |     100.8    |         6.3          |
| (far away)                  | 1024  &nbsp;&emsp;&emsp;&emsp;     |  16   |     12     |     104.1    |         6.5          |
|                             | 1500  &nbsp;&emsp;&emsp;&emsp;     |   ❌   |     ❌     |  ❌  |        N/A         |
| tierradelfuego.org.ar       | 64 &nbsp;&emsp;&emsp;&emsp;        |  22   |     22     |     395.5    |         18.0         |
| (very far away)             | 1024   &nbsp;&emsp;&emsp;&emsp;    |  22   |     22     |     405.7    |         18.4         |
|                             | 1500   &nbsp;&emsp;&emsp;&emsp;    |  29   |     31     |     547.3    |         19.2         |
| cocosseaview.com            | 64    &nbsp;&emsp;&emsp;&emsp;     |  22   |     22     |     510.0    |         23.2         |
| (small private hosting very far away) | 1024 &nbsp;&emsp;&emsp;&emsp; |  22   |     22     |     523.5    |         23.8         |
|                             | 1500   &nbsp;&emsp;&emsp;&emsp;     |  27   |     31     |     659.3    |         22.1         |


---------------------------------------------------------------------------------------------------------
| Service                     | Packet Size | Jumps | Jumps Back | Avg RTT (ms) | Avg RTT per Jump (ms) |
|-----------------------------|------------|-------|------------|--------------|------------------------|
| kiribati.gov.ki             | 64 B       |  27   |     17     |     415.3    |         15.4          |
| (very far away)             | 1024 B     |  27   |     17     |     428.7    |         15.9          |
|                             | 1500 B     |  25   |     16     |     481.3    |         17.8          |
| cs.pwr.edu.pl               | 64 B       |   5   |      2     |      76.3    |         15.3          |
| (very close)                | 1024 B     |   5   |      2     |       2.1    |          0.3          |
|                             | 1500 B     |   5   |      2     |       3.5    |          0.5          |
| olx.pl                      | 64 B       |   7   |     12     |      58.1    |          8.3          |
| (close)                     | 1024 B     |   7   |     12     |      61.2    |          8.7          |
|                             | 1500 B     |   ❌  |     ❌     |       ❌     |          ❌           |
| google.com                  | 64 B       |   6   |     18     |     136.4    |         22.7          |
| (big service,               | 1024 B     |   6   |     18     |     140.9    |         23.5          |
|  relatively close)          | 1500 B     |   ❌  |     ❌     |       ❌     |          ❌           |
| id.wikipedia.org            | 64 B       |  16   |     12     |     100.8    |          6.3          |
| (far away)                  | 1024 B     |  16   |     12     |     104.1    |          6.5          |
|                             | 1500 B     |   ❌  |     ❌     |      ❌      |          ❌           |
| tierradelfuego.org.ar       | 64 B       |  22   |     22     |     395.5    |         18.0          |
| (very far away)             | 1024 B     |  22   |     22     |     405.7    |         18.4          |
|                             | 1500 B     |  29   |     31     |     547.3    |         19.2          |
| cocosseaview.com            | 64 B       |  22   |     22     |     510.0    |         23.2          |
| (small private hosting,     | 1024 B     |  22   |     22     |     523.5    |         23.8          |
|  very far away)             | 1500 B     |  27   |     31     |     659.3    |         22.1          |
--------------------------------------------------------------------------------------------------------


pig github wilno