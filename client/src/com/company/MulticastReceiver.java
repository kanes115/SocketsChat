package com.company;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;

public class MulticastReceiver  implements Runnable {

    private final MulticastSocket mcSocket;

    public MulticastReceiver(MulticastSocket mcSocket, String mcIpString) throws IOException {
        this.mcSocket = mcSocket;
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
                DatagramPacket packet = new DatagramPacket(new byte[2048], 2048);

                mcSocket.receive(packet);
                String msg = new String(packet.getData(), packet.getOffset(),
                        packet.getLength());
                System.out.println(msg);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
