package com.company;

import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;

public class MulticastSender {

    private final String address;
    private final int mcPort;
    private final UdpSender udpSender;

    public MulticastSender(DatagramSocket socket, String address, int mcPort) throws UnknownHostException {
        this.address = address;
        this.mcPort = mcPort;
        this.udpSender = new UdpSender(socket, address, mcPort);
    }

    public void send(String msg){
        udpSender.send(msg);
    }
}
