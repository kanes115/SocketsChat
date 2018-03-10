package com.company;

import java.io.IOException;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.MulticastSocket;
import java.net.Socket;

public class Main {

    // Server's info
    private static int serverPort = 4003;
    private static String serverIp = "localhost";
    // On what port I listen to UDP packets (and also my host)
    private static int udpReceivePort = 6901;   //Should change among clients (configuration of udpReceivePort and myAddress
    private static String myAddress = "127.0.0.1";
    // Multicast info
    private static int mcPort = 9800;
    private static String mcIp = "224.0.0.1";

    public static void main(String[] args) {

        IoManager ioManager = new IoManager();
        ConfigBuilder configBuilder = new ConfigBuilder(ioManager.getInput("Name"), myAddress, udpReceivePort);

        try(Socket tcpSocket = new Socket(serverIp, serverPort);
            DatagramSocket udpSocket = new DatagramSocket(udpReceivePort);
            MulticastSocket mcSocket = new MulticastSocket(mcPort)
            ) {

            UdpSender udpSender = new UdpSender(udpSocket, serverIp, serverPort);
            TcpSender tcpSender = new TcpSender(tcpSocket);
            MulticastSender mcSender = new MulticastSender(udpSocket, mcIp, mcPort);

            Sender sender = new Sender(tcpSender, udpSender, mcSender, configBuilder.getConfig());

            TcpReceiver tcpReceiver = new TcpReceiver(tcpSocket);
            UdpReceiver udpReceiver = new UdpReceiver(udpSocket);
            MulticastReceiver mcReceiver = new MulticastReceiver(mcSocket, mcIp);


            new Thread(tcpReceiver).start();
            new Thread(udpReceiver).start();
            new Thread(mcReceiver).start();
            new Thread(sender).start();
            while(true)
                Thread.sleep(100);

        } catch (IOException | InterruptedException e) {
            e.printStackTrace();
        }

    }
}
