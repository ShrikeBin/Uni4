# **Understanding a Network Packet Capture**

A **network packet** is a small chunk of data sent over the internet or a local network. When you load a website, send an email, or make a video call, your device breaks the data into packets and sends them to the destination.

## **1. General Packet Information**
```
Frame 2: 53 bytes on wire (424 bits), 53 bytes captured (424 bits) on interface wlp5s0, id 0
```
- This packet is the **second** one in the capture.
- It has **53 bytes** of data (small packet).
- It was captured on the **wlp5s0** network interface (probably a Wi-Fi adapter).

---

## **2. Ethernet Layer (Data Link Layer)**
```
Ethernet II, Src: Intel_51:7a:89 (0c:7a:15:51:7a:89), Dst: Intel_f9:6b:db (cc:f9:e4:f9:6b:db)
```
- **Ethernet** is a networking protocol that helps devices communicate within a local network.
- **Src (Source)**: `0c:7a:15:51:7a:89` → The **MAC address** of the sender (like a unique ID for a network device).
- **Dst (Destination)**: `cc:f9:e4:f9:6b:db` → The **MAC address** of the receiver.

---

## **3. Internet Layer (IP - Internet Protocol)**
```
Internet Protocol Version 4, Src: 192.168.61.239, Dst: 192.168.61.232
```
- **IP (Internet Protocol)** helps data move from one device to another across networks.
- **Source IP**: `192.168.61.239` → The sender's address.
- **Destination IP**: `192.168.61.232` → The receiver's address.
- These addresses are **private** (used inside home/office networks).

Other IP details:
- **Time to Live (TTL)**: `64` → Limits how long the packet can travel before being discarded.
- **Protocol**: `ICMP (1)` → Indicates this is an ICMP (ping) packet.

---

## **4. Transport Layer (ICMP - Internet Control Message Protocol)**
```
Internet Control Message Protocol
    Type: 0 (Echo (ping) reply)
```
- **ICMP (Internet Control Message Protocol)** is used for network diagnostics.
- **Type: 0** → This is a **ping reply** (a response to a ping request), echo type, awaits anwser.
- **Checksum**: A way to verify if the packet was corrupted.

---

## **5. Data (Payload)**
```
Data (11 bytes)
    Data: 68656c6c6f20776f726c64
```
- The actual content of the message.
- This is hexadecimal (`68656c6c6f20776f726c64`), which translates to `"hello world"`.

---

## **Summary**
This is a **ping response** from one device to another within a private network (`192.168.61.x`). The message `"hello world"` was sent as a test. The packet traveled through the Ethernet and IP layers and was received successfully.

This kind of packet is useful for checking if two devices can communicate over a network.
