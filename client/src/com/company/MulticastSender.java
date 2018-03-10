package com.company;

import java.net.DatagramSocket;
import java.net.UnknownHostException;

public class MulticastSender {

    private final UdpSender udpSender;

    public MulticastSender(DatagramSocket socket, String address, int mcPort) throws UnknownHostException {
        this.udpSender = new UdpSender(socket, address, mcPort);
    }

    public void send(String msg){
        udpSender.send(msg);
    }
}
