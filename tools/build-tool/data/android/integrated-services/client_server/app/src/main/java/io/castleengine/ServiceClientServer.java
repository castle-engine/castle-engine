/*
  Copyright 2018-2020 Benedikt Magnus, Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
*/

package io.castleengine;

import android.os.Handler;
import android.os.Looper;
import android.util.Log;
import android.content.Context;

import java.net.*;
import java.io.*;
import java.util.*;
import java.util.concurrent.*;

public class ServiceClientServer extends ServiceAbstract
{
    private static final String CATEGORY = "ServiceClientServer";
    private static final String NAME = "client-server";

    private class ServerTuple
    {
        public Boolean active = true;
        public ConcurrentHashMap<String, Boolean> activeMap = null;

        public ServerTuple(ConcurrentHashMap<String, Boolean> setActiveMap)
        {
            activeMap = setActiveMap;
        }
    }

    private ConcurrentHashMap<String, ConcurrentHashMap<String, List<String>>> messageMap = new ConcurrentHashMap<String, ConcurrentHashMap<String, List<String>>>();
    private ConcurrentHashMap<String, ServerTuple> activeMap = new ConcurrentHashMap<String, ServerTuple>();

    public ServiceClientServer(MainActivity activity)
    {
        super(activity);
    }

    public String getName()
    {
        return NAME;
    }

    @Override
    public boolean messageReceived(String[] parts)
    {
        if ((parts.length < 4) || (parts.length > 6) || !parts[0].equals(NAME)) //client-server key action param1 (param2) (param3)
            return false;

        if (parts[2].equals("send")) //send message clientid
        {
            sendMessage(parts[1], parts[3], parts[4]);

            return true;
        }
        else if (parts[2].equals("server")) //server protocol port
        {
            if (!messageMap.containsKey(parts[1]))
                createSocket(parts[1], true, null, Integer.parseInt(parts[4]));

            return true;
        }
        else if (parts[2].equals("client")) //client protocol host port
        {
            if (!messageMap.containsKey(parts[1]))
                createSocket(parts[1], false, parts[4], Integer.parseInt(parts[5]));

            return true;
        }
        else if (parts[2].equals("close")) //close clientid
        {
            if (parts[3].equals("all"))
            {
                ConcurrentHashMap<String, Boolean> tempMap = activeMap.get(parts[1]).activeMap;
                synchronized (tempMap)
                {
                    for (String mapKey : tempMap.keySet())
                    {
                        tempMap.replace(mapKey, false);
                    }
                }
                activeMap.get(parts[1]).active = false;
            }
            else
            {
                activeMap.get(parts[1]).activeMap.replace(parts[3], false);
                messageMap.get(parts[1]).remove(parts[3]);
            }

            return true;
        }
        else
            return false;
    }

    private void createSocket(final String key, final Boolean isServer, final String host, final int port)
    {
        final ConcurrentHashMap<String, List<String>> socketMessageMap = new ConcurrentHashMap<String, List<String>>();
        final ConcurrentHashMap<String, Boolean> socketActiveMap = new ConcurrentHashMap<String, Boolean>();

        messageMap.put(key, socketMessageMap);
        activeMap.put(key, new ServerTuple(socketActiveMap));

        (new Thread(new Runnable()
            {
                @Override
                public void run()
                {
                    try
                    {
                        if (isServer)
                        {
                            Integer listenerId = 0;

                            ServerSocket serverSocket = new ServerSocket(port);
                            while (activeMap.get(key).active)
                            {
                                createListenerThread(serverSocket.accept(), key + Integer.toString(listenerId), socketMessageMap, socketActiveMap);
                                listenerId++;
                            }

                            serverSocket.close();
                            messageMap.remove(key);
                            activeMap.remove(key);
                        }
                        else
                            createListenerThread(new Socket(host, port), key + "client", socketMessageMap, socketActiveMap);
                    }
                    catch (IOException e)
                    {
                       logError(CATEGORY, e.toString());
                    }
                }
            }
        )).start();
    }

    private Boolean getOrDefault(ConcurrentHashMap<String, Boolean> aMap, String aValue, Boolean aDefault)
    {
        Boolean result = aMap.get(aValue);
        if (result == null)
            result = false;
        return result;
    }

    private void createListenerThread(final Socket listenerSocket, final String listenerId,
                                      final ConcurrentHashMap<String, List<String>> socketMessageMap, final ConcurrentHashMap<String, Boolean> socketActiveMap)
    {
        final List<String> listenerMessageList = Collections.synchronizedList(new ArrayList<String>());
        socketMessageMap.put(listenerId, listenerMessageList);
        socketActiveMap.put(listenerId, true);

        (new Thread(new Runnable()
            {
                @Override
                public void run()
                {
                    try
                    {
                        listenerSocket.setSoTimeout(100);

                        PrintWriter writer = new PrintWriter(listenerSocket.getOutputStream(), true);
                        BufferedReader reader = new BufferedReader(new InputStreamReader(listenerSocket.getInputStream()));

                        messageSendFromThread(new String[]{NAME, "connected", listenerId});

                        String inputLine;

                        while (getOrDefault(socketActiveMap, listenerId, false))
                        {
                            try
                            {
                                if ((inputLine = reader.readLine()) != null)
                                {
                                    messageSendFromThread(new String[]{NAME, "message", inputLine, listenerId});
                                }
                                else
                                {
                                    throw new IOException("Connection closed gracefully.");
                                }
                            }
                            catch (SocketTimeoutException e)
                            {
                                //Thrown when timeout is rechead. This is normal.
                            }

                            synchronized (listenerMessageList)
                            {
                                for (String message:listenerMessageList)
                                {
                                    listenerMessageList.remove(message);
                                    writer.println(message);
                                }
                            }
                        }
                    }
                    catch (IOException e)
                    {
                        logInfo(CATEGORY, e.toString());
                    }

                    messageSendFromThread(new String[]{NAME, "disconnected", listenerId});

                    try
                    {
                        listenerSocket.close();
                    }
                    catch (IOException e)
                    {
                        logInfo(CATEGORY, "Tried to close connection: " + e.toString());
                    }

                    socketActiveMap.remove(listenerId);
                }
            }
        )).start();
    }

    private void sendMessage(String key, String message, String clientId)
    {
        if (clientId.equals("all"))
        {
            ConcurrentHashMap<String, List<String>> tempMap = messageMap.get(key);
            synchronized (tempMap)
            {
                for (List<String> messageList : tempMap.values())
                {
                    synchronized (messageList)
                    {
                        messageList.add(message);
                    }
                }
            }
        }
        else
        {
            List<String> messageList;
            if ((messageList = messageMap.get(key).get(clientId)) != null)
                synchronized (messageList)
                {
                    messageList.add(message);
                }
        }
    }
}
