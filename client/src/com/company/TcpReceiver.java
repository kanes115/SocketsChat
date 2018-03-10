package com.company;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

public class TcpReceiver implements Runnable {

    private Socket socket;

    public TcpReceiver(Socket socket){
        this.socket = socket;
    }

    @Override
    public void run() {
        try {
            while(true) {
                BufferedReader in = new BufferedReader(new
                        InputStreamReader(socket.getInputStream()));
                String response = in.readLine();
                System.out.println(response);
            }
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(0);
        }
    }


}
