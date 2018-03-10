package com.company;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;

public class UdpReceiver implements Runnable {
    private DatagramSocket socket;

    public UdpReceiver(DatagramSocket socket){
        this.socket = socket;
    }

    @Override
    public void run() {
        byte[] receiveBuffer = new byte[2048];
        try {
            while(true) {
                DatagramPacket receivePacket =
                        new DatagramPacket(receiveBuffer, receiveBuffer.length);
                socket.receive(receivePacket);
                String msg = new String(receivePacket.getData());
                System.out.println("received on udp:\n" + msg);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
