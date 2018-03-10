package com.company;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.UnknownHostException;

public class MulticastReceiver implements Runnable {

    private final MulticastSocket mcSocket;
    private InetAddress mcAddr;

    public MulticastReceiver(MulticastSocket mcSocket, InetAddress mcAddr) {
        this.mcSocket = mcSocket;
        this.mcAddr = mcAddr;
    }


    @Override
    public void run() {
        return;

//        try{
//            mcSocket.joinGroup(mcAddr);

//            while(true){
//                DatagramPacket packet = new DatagramPacket(new byte[2048], 2048);

//                mcSocket.receive(packet);
//                String msg = new String(packet.getData(), packet.getOffset(),
//                        packet.getLength());
//                System.out.println(msg);
//            }

//        } catch (IOException e) {
//            e.printStackTrace();
//        }finally {
//        }

    }

}
