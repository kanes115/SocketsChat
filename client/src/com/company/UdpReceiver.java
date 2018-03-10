package com.company;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;

public class UdpReceiver implements Runnable {
    private DatagramSocket socket;
    private int bufferSize;

    public UdpReceiver(DatagramSocket socket, int bufferSize){
        this.socket = socket;
        this.bufferSize = bufferSize;
    }

    @Override
    public void run() {
        byte[] receiveBuffer = new byte[this.bufferSize];
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
            System.exit(0);
        }
    }
}
