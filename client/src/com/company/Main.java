package com.company;

import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.Socket;

public class Main {

    private static int serverPort = 4003;
    private static String serverIp = "localhost";
    private static int udpReceivePort = 6900;
    private static String myAddress = "127.0.0.1";

    public static void main(String[] args) {

        IoManager ioManager = new IoManager();
        ConfigBuilder configBuilder = new ConfigBuilder(ioManager.getInput("Name"), myAddress, udpReceivePort);

        try(Socket tcpSocket = new Socket(serverIp, serverPort);
            DatagramSocket udpSocket = new DatagramSocket(udpReceivePort)
            ) {

            UdpSender udpSender = new UdpSender(udpSocket, serverIp, serverPort);
            TcpSender tcpSender = new TcpSender(tcpSocket);

            Sender sender = new Sender(tcpSender, udpSender, configBuilder.getConfig());

            TcpReceiver tcpReceiver = new TcpReceiver(tcpSocket);
            UdpReceiver udpReceiver = new UdpReceiver(udpSocket);


            new Thread(tcpReceiver).start();
            new Thread(udpReceiver).start();
            new Thread(sender).start();
            while(true)
                Thread.sleep(100);

        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }

    }
}
