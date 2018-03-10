package com.company;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.UnknownHostException;

public class UdpSender {

    private DatagramSocket socket;
    private final InetAddress serverAddress;
    private final int serverPort;

    public UdpSender(DatagramSocket socket, String serverAddress, int serverPort) throws UnknownHostException {
        this.socket = socket;
        this.serverAddress = InetAddress.getByName(serverAddress);
        this.serverPort = serverPort;
    }

    public void send(String msg) {
        try {
            byte[] sendBuffer = msg.getBytes();
            DatagramPacket sendPacket;
            sendPacket = new DatagramPacket(sendBuffer, sendBuffer.length, serverAddress, serverPort);
            socket.send(sendPacket);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }



}
