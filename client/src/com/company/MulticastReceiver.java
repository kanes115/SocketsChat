package com.company;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;

public class MulticastReceiver  implements Runnable {

    private final MulticastSocket mcSocket;
    private final int bufferSize;

    public MulticastReceiver(MulticastSocket mcSocket, String mcIpString, int bufferSize) throws IOException {
        this.mcSocket = mcSocket;
        this.bufferSize = bufferSize;
        joinGroup(mcIpString);
    }


    private void joinGroup(String mcIpString) throws IOException {
        InetAddress mcIp = InetAddress.getByName(mcIpString);
        mcSocket.joinGroup(mcIp);
    }

    @Override
    public void run() {

        try {
            while(true) {
                DatagramPacket packet = new DatagramPacket(new byte[this.bufferSize], this.bufferSize);

                mcSocket.receive(packet);
                String msg = new String(packet.getData(), packet.getOffset(),
                        packet.getLength());
                System.out.println(msg);
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(0);
        }
    }
}
