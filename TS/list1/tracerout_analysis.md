# Traceroute Analysis

This document presents an analysis of traceroute results for three destinations: Google DNS (8.8.8.8), Kiribati Government (85.187.128.46), and the Wroclaw University of Science and Technology (217.173.198.55). Each trace provides insights into the network paths and latency between your machine and the respective destinations.

---

## Traceroute 1: Google DNS (8.8.8.8)

### Path Details:

1. **_gateway (192.168.61.197)** - 10.557 ms, 10.474 ms, 10.591 ms
2. **10.182.254.254 (10.182.254.254)** - 33.631 ms, 15.390 ms, 33.535 ms
3. **10.3.60.28 (10.3.60.28)** - 15.270 ms, 33.475 ms, 17.914 ms
4. **10.3.61.25 (10.3.61.25)** - 33.471 ms, 33.662 ms, 33.635 ms
5. **10.3.60.162 (10.3.60.162)** - 33.650 ms, 37.573 ms, 33.776 ms
6. **do-wcss.pwr.edu.pl (156.17.147.251)** - 33.286 ms, 21.333 ms, 24.078 ms
7. **156.17.252.52 (156.17.252.52)** - 24.157 ms, 24.413 ms, 24.368 ms
8. **156.17.252.53 (156.17.252.53)** - 23.822 ms, 13.518 ms, 13.433 ms
9. **156.17.254.101 (156.17.254.101)** - 12.218 ms, 18.488 ms, 16.495 ms
10. **156.17.251.0 (156.17.251.0)** - 24.880 ms, 24.814 ms, 24.844 ms
11. **z-Wroclaw-COM.poznan-gw2.rtr.pionier.gov.pl (212.191.239.42)** - 30.313 ms, 30.511 ms, 30.252 ms
12. **core1.ams.net.google.com (80.249.208.247)** - 46.661 ms, 47.377 ms, 58.233 ms
13. **74.125.243.125 (74.125.243.125)** - 51.072 ms, 74.125.242.181 (74.125.242.181) - 51.133 ms, 74.125.242.153 (74.125.242.153) - 36.543 ms
14. **142.250.224.133 (142.250.224.133)** - 36.194 ms, 216.239.49.13 (216.239.49.13) - 36.104 ms, 209.85.250.123 (209.85.250.123) - 36.072 ms
15. **dns.google (8.8.8.8)** - 35.159 ms, 33.694 ms, 33.596 ms

### Observations:
- Latency is relatively low with most hops under 50 ms.
- The final hop (Google DNS) shows a consistent latency of around 33 ms.

---

## Traceroute 2: Kiribati Government (85.187.128.46)

### Path Details:

1. **_gateway (192.168.61.197)** - 6.185 ms, 4.073 ms, 6.088 ms
2. **10.182.254.254 (10.182.254.254)** - 19.917 ms, 21.529 ms, 21.497 ms
3. **10.3.60.28 (10.3.60.28)** - 21.273 ms, 21.431 ms, 21.734 ms
4. **10.3.61.25 (10.3.61.25)** - 21.134 ms, 21.417 ms, 21.472 ms
5. **10.3.60.162 (10.3.60.162)** - 32.635 ms, 32.788 ms, 32.472 ms
6. **do-wcss.pwr.edu.pl (156.17.147.251)** - 30.630 ms, 26.898 ms, 9.673 ms
7. **156.17.252.52 (156.17.252.52)** - 11.253 ms, 21.302 ms, 21.238 ms
8. **156.17.252.53 (156.17.252.53)** - 20.266 ms, 21.173 ms, 21.284 ms
9. **156.17.254.101 (156.17.254.101)** - 21.433 ms, 21.404 ms, 8.213 ms
10. **z-Wroclaw.poznan-gw7.rtr.pionier.gov.pl (212.191.225.42)** - 11.578 ms, 11.711 ms, 11.709 ms
11. **pionier-ias-geant-gw-1.poz.pl.geant.net (83.97.88.121)** - 15.233 ms, 19.442 ms, 19.496 ms
12-15. **No response**
16. **ae3-0.mx1.lon.uk.geant.net (62.40.98.61)** - 32.149 ms, 31.695 ms, 29.508 ms
17. **195.66.225.227 (195.66.225.227)** - 31.737 ms, 45.867 ms, 44.895 ms
18-19. **No response**
20. **ae19.mcs1.lhr15.uk.zip.zayo.com (64.125.24.69)** - 32.088 ms, 31.195 ms, 37.482 ms
21. **82.98.196.161.IPYX-284545-900-ZYO.zip.zayo.com (82.98.196.161)** - 100.998 ms, 44.557 ms, 47.248 ms
22. **i-10070.ulcn-core01.telstraglobal.net (202.84.178.53)** - 46.023 ms, 74.943 ms, 45.839 ms
23-24. **High Latency (303 ms)**
25. **unknown.telstraglobal.net (210.57.30.82)** - 303.415 ms, 303.387 ms, 280.355 ms
26. **e1-1.SG1-C1-A10011_0109-35.a2webhosting.com (69.48.139.131)** - 269.131 ms, 268.697 ms, 267.862 ms
27. **sg1-tr2.supercp.com (85.187.128.46)** - 264.902 ms, 304.775 ms, 304.701 ms

### Observations:
- The latency spikes significantly in the middle of the trace, especially in hops 22–27.
- The response time increases after crossing into higher-latency networks, which is expected for long-distance routes.

---

## Traceroute 3: Wroclaw University of Science and Technology (217.173.198.55)

### Path Details:

1. **_gateway (192.168.61.197)** - 31.022 ms, 39.753 ms, 39.716 ms
2. **10.182.254.254 (10.182.254.254)** - 81.598 ms, 81.559 ms, 81.572 ms
3. **10.3.60.28 (10.3.60.28)** - 81.486 ms, 81.457 ms, 81.427 ms
4. **10.3.61.25 (10.3.61.25)** - 81.399 ms, 81.420 ms, 81.339 ms
5. **10.3.60.162 (10.3.60.162)** - 81.309 ms, 131.846 ms, 131.818 ms
6. **do-wcss.pwr.edu.pl (156.17.147.251)** - 131.788 ms, 41.628 ms, 37.517 ms
7. **156.17.252.52 (156.17.252.52)** - 37.444 ms, 37.405 ms, 37.370 ms
8. **156.17.252.53 (156.17.252.53)** - 101.157 ms, 101.123 ms, 101.089 ms
9. **156.17.254.101 (156.17.254.101)** - 101.056 ms, 101.024 ms, 100.992 ms
10. **opole-do-wroclaw.wask.wroc.pl (156.17.251.169)** - 147.972 ms, 147.936 ms, 146.727 ms
11. **217.173.193.218 (217.173.193.218)** - 146.632 ms, 146.595 ms, No Response
12-59. **No Response**

### Observations:
- Similar to the Kiribati trace, there is a noticeable delay when crossing through multiple hops (increasing from 81 ms to 131 ms and then jumping to 146 ms).
- The trace ends without response beyond hop 11, which could indicate a network issue or drop beyond that point.

---

## Conclusion:

- **Google DNS** had the best performance with low latency.
- **Kiribati** and **Wroclaw University** showed higher latencies, with some lost packets and significant latency spikes in the middle of the trace.
- For long-distance connections (such as Kiribati), latency increases as the trace crosses international networks.
