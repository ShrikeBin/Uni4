# Network Ping Analysis

## 1. Ping to `kiribati.gov.ki` (85.187.128.46)
- **Location**: Kiribati (Pacific Ocean region)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 334.8ms / 415.3ms / 472.4ms
  - **TTL**: failed at < 27, back 17
  - **Packet Loss**: 0%
- **Analysis**: This server is quite distant from Central Europe, as Kiribati is in the Pacific. The latency is high, and the number of hops is likely greater due to the need to route across multiple international networks and undersea cables. A failed TTL value of 27 suggests around 30 hops.

## 2. Ping to `cs.pwr.edu.pl` (156.17.7.22)
- **Location**: Poland (Wrocław)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 44.1ms / 76.3ms / 123.9ms
  - **TTL**: failed at < 2 back 2
  - **Packet Loss**: 0%
- **Analysis**: This is a nearby location in Central Europe. The average RTT is relatively low, and the failed TTL of 5 indicates around 4-6 hops, showing a relatively direct path within Europe.

## 3. Ping to `olx.pl` (18.244.102.127)
- **Location**: Poland (Warsaw)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 36.2ms / 58.1ms / 78.1ms
  - **TTL**: 7 back 12
  - **Packet Loss**: 0%
- **Analysis**: This server is also located in Poland. The RTT is lower than the Wrocław server, and the TTL of 7 suggests that it’s on a content delivery network (CDN) or optimized routing, reducing latency. The higher TTL suggests more hops are involved, likely from intermediary CDN servers.

## 4. Ping to `google.com` (142.250.179.142)
- **Location**: Likely a Google server in the Netherlands or the US
- **Ping Results**:
  - **Min/Avg/Max RTT**: 93.5ms / 136.4ms / 235ms
  - **TTL**: failed at 6 back 18
  - **Packet Loss**: 0%
- **Analysis**: Google's servers are globally distributed, and this server is likely in Europe, given the TTL. The failed TTL of 6 suggests that the hop count to this server is around 6, and latency varies with high packet variation, possibly due to dynamic routing or load balancing.

## 5. Ping to `id.wikipedia.org` (185.15.59.224)
- **Location**: Wikimedia servers (likely distributed in Europe or the US)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 79.8ms / 100.8ms / 118ms
  - **TTL**: failed at 16 back 12
  - **Packet Loss**: 40%
- **Analysis**: A reasonably low RTT, suggesting proximity to Central Europe. The failed TTL of 16 indicates around 16 hops. The 40% packet loss could indicate congestion, routing inefficiencies, or issues with a specific network segment.

## 6. Ping to `tierradelfuego.org.ar` (200.0.230.196)
- **Location**: Argentina (Southernmost region)
- **Ping Results**:
  - **Min/Avg/Max RTT**: 352.4ms / 395.5ms / 533.3ms
  - **TTL**: failed at 22 back 22
  - **Packet Loss**: 0%
- **Analysis**: Argentina’s distance from Europe results in high RTT, but this is still a reasonable range for transatlantic communication. The failed TTL at 22 indicates around 22 hops, suggesting the use of multiple intermediate networks, possibly routed through South American or US networks.

## 7. Ping to `cocosseaview.com` (202.164.22.100)
- **Location**: Hotel in the Indian Ocean
- **Ping Results**:
  - **Min/Avg/Max RTT**: 392ms / 510ms / 585ms
  - **TTL**: failed at 22 back 22
  - **Packet Loss**: 0%
- **Analysis**: This server is located in the Indian Ocean, resulting in a relatively high RTT, but within a reasonable range for communication across the Indian Ocean. The failed TTL at 22 indicates around 22 hops, which is expected for routing through undersea cables or regional networks. The packet loss is minimal, suggesting stable connectivity to this remote location.
