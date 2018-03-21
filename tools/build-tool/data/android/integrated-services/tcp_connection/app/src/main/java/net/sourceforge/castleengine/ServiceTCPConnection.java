package net.sourceforge.castleengine;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.content.Context;

import java.net.*;
import java.io.*;

public class ServiceTCPConnection extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceTCPConnection";

    private PrintWriter writer = null;
    private Boolean running = false;

    public ServiceTCPConnection(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return "tcp_connection";
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if ((parts.length < 2) || (parts.length > 4) || !parts[0].equals("tcp_connection"))
            return false;
        try
        {
            if (parts[1].equals("send")) //send message
            {
                SendMessage(parts[2]);
                return true;
            }
            else if (parts[1].equals("server")) //server port
            {
                CreateServer(Integer.parseInt(parts[2]));
                return true;
            }
            else if (parts[1].equals("client")) //client host port
            {
                CreateClient(parts[2], Integer.parseInt(parts[3]));
                return true;
            }
            else if (parts[1].equals("close")) //close
            {
                running = false;
                return true;
            }
            else
                return false;
        }
        catch (IOException e)
        {
            System.err.println(e);
            return false;
        }
    }

    private void CreateServer (int port) throws IOException 
    {
            ServerSocket serverSocket = new ServerSocket(port);
            Socket clientSocket = serverSocket.accept();
            CreateSocketAndListener(clientSocket); 
    }

    private void CreateClient (String host, int port) throws IOException 
    {
        Socket clientSocket = new Socket(host, port);
        CreateSocketAndListener(clientSocket);
    }

    private void CreateSocketAndListener (final Socket clientSocket) throws IOException 
    {
        running = true;

        Thread thread = new Thread(new Runnable()
            {
                @Override
                public void run()
                {
                    try
                    {
                        writer = new PrintWriter(clientSocket.getOutputStream(), true);
                        BufferedReader in = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));
                        String fromServer;
                        String fromUser;

                        String inputLine;

                        while (running)
                        {
                            if ((inputLine = in.readLine()) != null)
                            {
                                MessageSendSynchronised(inputLine);
                            }
                        }
                    }
                    catch (IOException e)
                    {
                        System.err.println(e);
                    }

                    writer = null;
                }
            }
        );

        thread.start();
    }

    private void SendMessage (String message)
    {
        writer.println(message);
    }

    private void MessageSendSynchronised (final String message)
    {
        new Handler(Looper.getMainLooper()).post(new Runnable() // run in main thread
            {   
                @Override
                public void run()
                {
                    messageSend(new String[]{"tcp_message", message});
                }
            }
        );
    }
}
