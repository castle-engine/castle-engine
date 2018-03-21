package net.sourceforge.castleengine;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.content.Context;

import java.net.*;
import java.io.*;
import java.util.*;

public class ServiceTCPConnection extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceTCPConnection";

    private PrintWriter writer = null;
    private Boolean running = false;
    private List<String> messageList = Collections.synchronizedList(new ArrayList<String>());

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
                CreateSocketAndListener(true, null, Integer.parseInt(parts[2]));
                return true;
            }
            else if (parts[1].equals("client")) //client host port
            {
                CreateSocketAndListener(false, parts[2], Integer.parseInt(parts[3]));
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

    private void CreateSocketAndListener (final Boolean isServer, final String host, final int port) throws IOException 
    {
        running = true;

        Thread thread = new Thread(new Runnable()
            {
                @Override
                public void run()
                {
                    try
                    {
                        Socket clientSocket;

                        if (isServer)
                        {
                            ServerSocket serverSocket = new ServerSocket(port);
                            clientSocket = serverSocket.accept();
                        }
                        else
                            clientSocket = new Socket(host, port);

                        clientSocket.setSoTimeout(200);

                        writer = new PrintWriter(clientSocket.getOutputStream(), true);
                        BufferedReader reader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

                        MessageSendSynchronised(new String[]{"tcp_connected"});

                        String inputLine;

                        while (running)
                        {
                            try
                            {
                                if ((inputLine = reader.readLine()) != null)
                                {
                                    MessageSendSynchronised(new String[]{"tcp_message", inputLine});
                                }
                            }
                            catch (SocketTimeoutException e)
                            {
                                //Thrown when timeout is rechead. This is normal. 
                            }

                            synchronized (messageList)
                            {
                                for (String message:messageList)
                                {
                                    writer.println(message);
                                    messageList.remove(message);
                                }
                            }
                            
                        }

                        clientSocket.close();
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
        synchronized (messageList)
        {
            messageList.add(message);
        }
        //if (writer != null)
        //    writer.println(message);
    }

    private void MessageSendSynchronised (final String[] message)
    {
        new Handler(Looper.getMainLooper()).post(new Runnable() // run in main thread
            {   
                @Override
                public void run()
                {
                    messageSend(message);
                }
            }
        );
    }
}
