package com.company;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.*;
import java.util.Scanner;

public class Sender implements Runnable {

    private final IoManager ioManager;
    private TcpSender tcpSender;
    private String config;
    private UdpSender udpSender;
    private PrintWriter out;

    public Sender(TcpSender tcpSender, UdpSender udpSender, String config) throws IOException {
        this.tcpSender = tcpSender;
        this.config = config;
        this.ioManager = new IoManager();
        this.udpSender = udpSender;
    }

    @Override
    public void run() {
        tcpSend(config);
        try{
            while(true) {
                String msg = ioManager.getInput();
                if(msg.equals("\\U"))
                    sendMedia();
                else
                    tcpSend(msg);
            }
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void tcpSend(String msg){
        tcpSender.send(msg);
    }

    private void sendMedia(){
        udpSender.send("PingPong");
    }

}
