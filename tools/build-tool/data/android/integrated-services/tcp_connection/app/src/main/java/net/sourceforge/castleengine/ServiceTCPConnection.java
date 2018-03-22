package net.sourceforge.castleengine;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.content.Context;

import java.net.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

public class ServiceTCPConnection extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceTCPConnection";

    private ConcurrentHashMap<String, Boolean> activeMap = new ConcurrentHashMap<String, Boolean>();
    private ConcurrentHashMap<String, List<String>> messageMap = new ConcurrentHashMap<String, List<String>>();

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
        if ((parts.length < 3) || (parts.length > 5) || !parts[0].equals("tcp_connection")) //tcp_connection key action param1 (param2)
            return false;
        try
        {
            if (parts[2].equals("send")) //send message
            {
                SendMessage(parts[1], parts[3]);
                return true;
            }
            else if (parts[2].equals("server")) //server port
            {
                if (!messageMap.containsKey(parts[1]))
                    CreateSocketAndListener(parts[1], true, null, Integer.parseInt(parts[3]));

                return true;
            }
            else if (parts[2].equals("client")) //client host port
            {
                if (!messageMap.containsKey(parts[1]))
                    CreateSocketAndListener(parts[1], false, parts[3], Integer.parseInt(parts[4]));

                return true;
            }
            else if (parts[2].equals("close")) //close
            {
                activeMap.replace(parts[1], false);
                messageMap.remove(parts[1]);

                return true;
            }
            else
                return false;
        }
        catch (IOException e)
        {
            System.err.println(e);
            return true;
        }
    }

    private void CreateSocketAndListener (final String key, final Boolean isServer, final String host, final int port) throws IOException 
    {
        final List<String> messageList = Collections.synchronizedList(new ArrayList<String>());

        messageMap.put(key, messageList);
        activeMap.put(key, true);

        (new Thread(new Runnable()
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

                        clientSocket.setSoTimeout(100);

                        PrintWriter writer = new PrintWriter(clientSocket.getOutputStream(), true);
                        BufferedReader reader = new BufferedReader(new InputStreamReader(clientSocket.getInputStream()));

                        MessageSendSynchronised(new String[]{"tcp_connected"});

                        String inputLine;

                        while (activeMap.get(key))
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

                        activeMap.remove(key);
                    }
                    catch (IOException e)
                    {
                        System.err.println(e);
                    }
                }
            }
        )).start();
    }

    private void SendMessage (String key, String message)
    {
        List<String> messageList;
        if ((messageList = messageMap.get(key)) != null)
            synchronized (messageList)
            {
                messageList.add(message);
            }
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
