package com.company;

public class ConfigBuilder {

    private final String name;
    private final String address;
    private final int udpReceiverPort;
    private final String delimeter = ":";

    public ConfigBuilder(String name, String address, int udpReceiverPort){
        this.name = name;
        this.address = address;
        this.udpReceiverPort = udpReceiverPort;
    }

    public String getConfig(){
        return this.name + this.delimeter + this.address + this.delimeter + this.udpReceiverPort;
    }
}
