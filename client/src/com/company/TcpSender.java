package com.company;

import java.io.IOException;
import java.io.PrintWriter;
import java.net.Socket;

public class TcpSender {

    private Socket socket;
    private PrintWriter out;

    public TcpSender(Socket socket) throws IOException {
        this.socket = socket;
        out = new PrintWriter(socket.getOutputStream(), true);
    }


    public void send(String msg){
        out.println(msg);
    }
}
