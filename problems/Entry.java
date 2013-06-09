import py4j.GatewayServer;
/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

/**
 *
 * @author derricklin
 * This is for the py4j attempt, which is so close!
 */
public class Entry {

    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        GatewayServer gatewayServer = new GatewayServer(null);
        gatewayServer.start();
        System.out.println("Gateway Server Started");

    }
}
