Unit TERRA_AIBehavior;
{$I terra.inc}
// http://www.red3d.com/cwr/steer/gdc99/

Interface

Uses TERRA_Utils, TERRA_Vector2D, TERRA_Collision2D, TERRA_Color, TERRA_Image,
  TERRA_AIPath, TERRA_Math, TERRA_Application;

Const
  // single behaviors
  aiBehaviorStop                = 0;
  aiBehaviorPursuit             = 1;
  aiBehaviorEvasion             = 2;
  aiBehaviorArrival             = 3;
  aiBehaviorWander              = 4;
  aiBehaviorPathFollowing       = 5;
  aiBehaviorWallFollowing       = 6;
  aiBehaviorContainment         = 7;
  aiBehaviorFlowFieldFollowing  = 8;

  // crowd hehaviors
  aiBehaviorSeparation          = 9;
  aiBehaviorCohesion            = 10;
  aiBehaviorAlignment           = 11;
  aiBehaviorLeaderFollowing     = 13;
  aiBehaviorHide                = 14;

Type
  AIVehicle = Class;

  AIObstacle = Class
    Public
      Function Collision(VehicleHitArea:Circle; Hit:PVector2D):Boolean; Virtual; Abstract;
      Procedure Render; Virtual; Abstract;
  End;

  AIPolygonObstacle = Class(AIObstacle)
    Protected
      _Polygon:Polygon2D;

    Public
      Constructor Create(Poly:Polygon2D);

      Function Collision(VehicleHitArea:Circle; Hit:PVector2D):Boolean; Override;
      Procedure Render; Override;
  End;

  AIVehicle = Class
    Protected
      _Behavior:Integer;
      _Target:Vector2D;
      _Velocity:Vector2D;
      _Steer:Vector2D;
      _Wander:Single;

    Public
      Position:Vector2D;
      Radius:Single;
      Range:Single;
      MaxSpeed:Single;

      Constructor Create;

      Procedure SetBehavior(BehaviorType:Integer);
      Procedure SetTarget(NewTarget:Vector2D);

      Function GetCollisionArea:Circle;

      Procedure Update(Delta:Single);

      //Procedure Render;

      Property Target:Vector2D Read _Target Write SetTarget;
  End;

  AIManager = Class(ApplicationComponent)
    Protected
      _LastUpdate:Cardinal;

      _Vehicles:Array Of AIVehicle;
      _VehicleCount:Integer;

      _Obstacles:Array Of AIObstacle;
      _ObstacleCount:Integer;

      Procedure Init; Override;
      Procedure Update; Override;

      Class Function GetUntypedInstance():ApplicationComponent; Override;

    Public
      DebugMode:Boolean;
      AreaMin:Vector2D;
      AreaMax:Vector2D;

      Destructor Destroy;

      Procedure Clear;

      Procedure AddVehicle(Vehicle:AIVehicle);
      Procedure DeleteVehicle(Vehicle:AIVehicle);

      Procedure AddObstacle(Obstacle:AIObstacle);
      Procedure DeleteObstacle(Obstacle:AIObstacle);

      Function TestCollision(CollisionBody:Circle):Boolean;

      Class Function Instance:AIManager;

    End;

Implementation
Uses TERRA_OS;

Var
  _AIManager_Instance:AIManager;

{ AIManager }

Constructor AIManager.Create;
{Var
  Gen:PerlinNoiseGenerator;}
Begin
  {Gen := PerlinNoiseGenerator.Create;
  _Noise := Gen.CreateImage(64, 64);
  Gen.Destroy;}

  _LastUpdate := GetTime;
  AreaMin := VectorCreate2D(-9999, -9999);
  AreaMax := VectorCreate2D(9999, 9999);
End;

Destructor AIManager.Destroy;
Begin
  {If Assigned(_Noise) Then
    _Noise.Destroy;}
    
  _AIManager_Instance := Nil;
End;

Class Function AIManager.Instance:AIManager;
Begin
  If Not Assigned(_AIManager_Instance) Then
    _AIManager_Instance := AIManager.Create;

  Result := _AIManager_Instance;
End;

Procedure AIManager.AddVehicle(Vehicle: AIVehicle);
Begin
  Inc(_VehicleCount);
  SetLength(_Vehicles, _VehicleCount);
  _Vehicles[Pred(_VehicleCount)] := Vehicle;
End;

Procedure AIManager.DeleteVehicle(Vehicle: AIVehicle);
Var
  N, I:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_VehicleCount) Do
  If (_Vehicles[I]=Vehicle) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _Vehicles[N] := _Vehicles[Pred(_VehicleCount)];
  Dec(_VehicleCount);
End;

Procedure AIManager.AddObstacle(Obstacle: AIObstacle);
Begin
  Inc(_ObstacleCount);
  SetLength(_Obstacles, _ObstacleCount);
  _Obstacles[Pred(_ObstacleCount)] := Obstacle;
End;

Procedure AIManager.DeleteObstacle(Obstacle: AIObstacle);
Var
  N, I:Integer;
Begin
  N := -1;
  For I:=0 To Pred(_ObstacleCount) Do
  If (_Obstacles[I]=Obstacle) Then
  Begin
    N := I;
    Break;
  End;

  If (N<0) Then
    Exit;

  _Obstacles[N] := _Obstacles[Pred(_ObstacleCount)];
  Dec(_ObstacleCount);
End;

Procedure AIManager.Clear;
Begin
  _VehicleCount := 0;
  _ObstacleCount := 0;
End;

Function AIManager.TestCollision(CollisionBody: Circle): Boolean;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_ObstacleCount) Do
  If  (_Obstacles[I].Collision(CollisionBody, Nil)) Then
  Begin
    Result := True;
    Exit;
  End;

  For I:=0 To Pred(_VehicleCount) Do
  If (_Vehicles[I].GetCollisionArea.Intersect(CollisionBody)) Then
  Begin
    Result := True;
    Exit;
  End;

  Result := False;
End;

Procedure AIManager.OnAppUpdate;
Var
  Delta:Single;
  T:Cardinal;
  I:Integer;
Begin
  T := GetTime;
  Delta := (T - _LastUpdate) / 100;
  _LastUpdate := T;

  For I:=0 To Pred(_VehicleCount) Do
    _Vehicles[I].Update(Delta);
End;

{ AIVehicle }

Constructor AIVehicle.Create;
Begin
  Radius := 1.0;
  Range := 100;
  MaxSpeed := 5;
End;

Function AIVehicle.GetCollisionArea:Circle;
Begin
  Result.Center := Self.Position;
  Result.Radius := Self.Radius;
End;

Procedure AIVehicle.SetBehavior(BehaviorType: Integer);
Begin
  _Behavior := BehaviorType;
End;

Procedure AIVehicle.SetTarget(NewTarget:Vector2D);
Begin
  If (_Behavior = aiBehaviorWander) Then
    Exit;

  _Target := NewTarget;
End;

{Procedure AIVehicle.Render;
Var
  L:Line2D;
  C:Circle;
Begin
  C := Self.GetCollisionArea;
  C.Draw(ColorBlue);

  If Not _AIManager_Instance.DebugMode Then
    Exit;

  L.P1 := Self.Position;
  L.P2 := _Velocity;
  L.P2.Scale(100);
  L.P2.Add(L.P1);
  L.Draw(ColorWhite);

  L.P1 := Self.Position;
  L.P2 := _Steer;
  L.P2.Add(L.P1);
  L.Draw(ColorRed);
End;}

Procedure AIVehicle.Update(Delta:Single);
Var
  I, MinIndex:Integer;
  H, Hit:Vector2D;
  Dir:Line2D;
  Min, D:Single;
  Collision:Boolean;
  Speed, Distance:Single;
  U,V:Single;
  TX,TY:Integer;
  Cl:Color;
Begin
  _Velocity.Scale(0.0);

  If (_Behavior = aiBehaviorStop) Then
    Exit;

  Collision := False;
  Speed := MaxSpeed;

  For I:=0 To Pred(_AIManager_Instance._ObstacleCount) Do
  If  (_AIManager_Instance._Obstacles[I].Collision(Self.GetCollisionArea, @Hit)) Then
  Begin
    _Steer := Hit;
    _Steer.Subtract(Self.Position);
    _Steer.Normalize;
    _Steer.Scale(-(Self.Radius+_AIManager_Instance._Vehicles[I].Radius));
    _Velocity := _Steer;
    Collision := True;
    Break;
  End;

  If Not Collision Then
  For I:=0 To Pred(_AIManager_Instance._VehicleCount) Do
  If (_AIManager_Instance._Vehicles[I]<>Self) And (Self.GetCollisionArea.Intersect(_AIManager_Instance._Vehicles[I].GetCollisionArea)) Then
  Begin
    _Steer := _AIManager_Instance._Vehicles[I].Position;
    _Steer.Subtract(Self.Position);
    _Steer.Normalize;
    _Steer.Scale(-(Self.Radius+_AIManager_Instance._Vehicles[I].Radius));
    _Velocity := _Steer;
    Collision := True;
    Break;
  End;

  {
  If (Not Collision) Then
  Begin
    If (Position.X<_AIManager_Instance.AreaMin.X+Radius) Or (Position.X>_AIManager_Instance.AreaMax.X-Radius) Then
    Begin
      Collision := True;
      _Velocity.X := FixMul(_AIManager_Instance.AreaMin.X - Position.X, Radius);
    End;

    If (Position.Y<_AIManager_Instance.AreaMin.Y+Radius) Or (Position.Y>_AIManager_Instance.AreaMax.Y-Radius) Then
    Begin
      Collision := True;
      _Velocity.Y := FixMul(_AIManager_Instance.AreaMin.Y - Position.Y, Radius);
    End;
  End; }

      If (Not Collision) Then
      Begin

        If (_Behavior = aiBehaviorWander) Then
        Begin
          _Wander := _Wander + Delta;
          If (_Target.Distance(Self.Position)< (Self.Radius * 2)) Or (_Wander>(Range*Range)) Then
          Begin
            _Wander := 0.0;
            _Target := VectorCreate2D(RandomFloat(_AIManager_Instance.AreaMin.X, _AIManager_Instance.AreaMax.X), RandomFloat(_AIManager_Instance.AreaMin.Y, _AIManager_Instance.AreaMax.Y));
          End;
        End;

          _Velocity := _Target;
          _Velocity.Subtract(Position);
          If (_Behavior = aiBehaviorEvasion) Then
            _Velocity.Scale(-1.0);

          Distance := _Velocity.Length;
         If (_Behavior = aiBehaviorArrival) Then
         Begin
           If (Distance>Self.Range) Then
             Distance := Self.Range;

           Speed := Speed * (Distance / Self.Range);
          End;
        End;

        H := _Velocity;
        H.Scale(Self.Radius * 2);

        Dir.P1 := Position;
        Dir.P2 := Dir.P1;
        Dir.P2.Add(H);

        _Steer.Scale(0.0);
        Min := 9999;
        MinIndex := -1;

        For I:=0 To Pred(_AIManager_Instance._VehicleCount) Do
        If (_AIManager_Instance._Vehicles[I]<>Self) And (Dir.Intersect(_AIManager_Instance._Vehicles[I].GetCollisionArea, @Hit)) Then
        Begin
          _Steer := _AIManager_Instance._Vehicles[I].Position;
          _Steer.Subtract(Self.Position);
          _Steer.Normalize;
          _Steer.Scale(0.0005); //-(Self.Radius + _AIManager_Instance._Vehicles[MinIndex].Radius));
          _Velocity.Add(_Steer);

          {D := Hit.Distance(Self.Position);
          If (D<Min) Then
          Begin
            Min := D;
            MinIndex := I;
          End;}
        End;

        {If (MinIndex>=0) Then
        Begin
          _Steer := _AIManager_Instance._Vehicles[MinIndex].Position;
          _Steer.Subtract(Self.Position);
          _Steer.Normalize;
          _Steer.Scale(0.0005); //-(Self.Radius + _AIManager_Instance._Vehicles[MinIndex].Radius));
          _Velocity.Add(_Steer);
        End;}

  _Velocity.Normalize;
  _Velocity.Scale(Delta * Speed);
  Position.Add(_Velocity);
End;

{ AIPolygonObstacle }


Constructor AIPolygonObstacle.Create(Poly: Polygon2D);
Begin
  _Polygon := Poly;
End;

Procedure AIPolygonObstacle.Render;
Begin
  //_Polygon.Draw(ColorWhite);
End;

Function AIPolygonObstacle.Collision(VehicleHitArea: Circle; Hit:PVector2D): Boolean;
Begin
  Result := _Polygon.Intersect(VehicleHitArea, Hit);
End;

Initialization
  RegisterApplicationComponent(AIManager.Instance, Nil);
End.
