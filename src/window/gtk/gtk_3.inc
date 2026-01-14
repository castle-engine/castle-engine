{%MainUnit ../castlewindow.pas}

{ Definitions specific to GTK 3 (and not used with GTK 2). }

const
  { Define gfalse, gtrue just like GTK 2 does.
    In case of GTK 3 bindings, gboolean is just Boolean32. }
  gfalse = gboolean(0);
  gtrue = gboolean(1);

type
  { These are functions that check type in GTK 2.
    In GTK 3, we just cast.
    TODO: use g_type_check_instance_cast to perform check and cast? }
  GTK_WIDGET = PGtkWidget;
  GTK_OBJECT = PGObject;
  GTK_WINDOW = PGtkWindow;
  GTK_CONTAINER = PGtkContainer;
  GTK_SIGNAL_FUNC = TGCallback;
  G_OBJECT = PGObject;
  GTK_DIALOG = PGtkDialog;

{ Implement a few methods that match GTK 2 names and signature
  using straightforward counterparts from GTK 3. }

function gtk_signal_connect(const Obj: PGObject; const SignalName: String;
  const CHandler: TGCallback; const Data: Pointer): GULong;
begin
  Result := g_signal_connect_data(Obj, PAnsiChar(SignalName), CHandler, Data, nil, G_CONNECT_DEFAULT);
end;

procedure gtk_signal_disconnect(const Obj: PGObject; const HandlerId: GULong);
begin
  g_signal_handler_disconnect(Obj, HandlerId);
end;

procedure gtk_object_set_user_data(const Obj: PGObject; const Data: Pointer);
begin
  g_object_set_data(Obj, 'castle_user_data', Data);
end;

function gtk_object_get_user_data(const Obj: PGObject): Pointer;
begin
  Result := g_object_get_data(Obj, 'castle_user_data');
end;

function gtk_events_pending_bool: Boolean;
begin
  Result := gtk_events_pending();
end;
