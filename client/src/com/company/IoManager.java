package com.company;

import java.util.Scanner;

public class IoManager {


    Scanner reader = new Scanner(System.in);

    public String getInput(){
        String msg;
        msg = reader.nextLine();
        return msg;
    }

    public String getInput(String prompt){
        System.out.print(prompt + ": ");
        return getInput();
    }
}
