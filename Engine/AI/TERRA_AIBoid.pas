Unit TERRA_AIBoid;

{$I terra.inc}
Interface

Uses TERRA_Object, TERRA_Utils, TERRA_Vector3D, TERRA_Vector2D, TERRA_Matrix4x4, TERRA_Color, TERRA_Mesh, TERRA_Viewport;

Const
   BoidSpeed = 20;

Type
  BoidAgent = Class;
  BoidGene = Class;

  BoidFlock = Class(TERRAObject)
    Private
      _MemberCount:integer;	                              // Number of critters on each team.
      _members:Array Of BoidAgent;                                 // The critters.
      _meanHeading:Single;
      _meanSpeed:Single;
      _meanX:Single;
      _meanY:Single;        // Flock averages.

      // world size
      _height:Single;
      _width:Single;

      _TeamCount:Integer;                                  // Number of teams.
      _Teams:Array Of TERRAMesh;

      procedure checkBounds( boid : BoidAgent);
      procedure addToMomentum( b : BoidAgent);


      function FindAngle( FSourceX, FSourceY, FDestX, FDestY  : single) : single;

    Public

      Constructor Create(size: integer; h, w : Single);

      Procedure Release; Override;


      Procedure AddTeam(Mesh:TERRAMesh);

      procedure Update;

      function calculateMeanX : Single;
      function calculateMeanY : Single;

      procedure initializeDuel( gene, challenger : BoidGene);
      procedure add( b : BoidAgent);
      procedure addBoid(direction,
                        magnitude,
                        xPos,
                        yPos,
                        heightOfs,
                        maxSpeed,
                        minSpeed,
                        angle,
                        accel,
                        sensor,
                        death,
                        deathC,
                        collision,
                        attackC,
                        retreatC: Single;
                        TeamIndex: integer);

      procedure Remove(b:BoidAgent);
      //function elements : TList;

      procedure Render(View:TERRAViewport);


      Property MemberCount:Integer Read _MemberCount;
      Property TeamCount:Integer Read _TeamCount;

      Property MeanHeading:Single Read _MeanHeading Write _MeanHeading;
      Property MeanSpeed:Single Read _MeanSpeed Write _MeanSpeed;

      Property MeanX:Single Read _MeanX Write _MeanX;
      Property MeanY:Single Read _MeanY Write _MeanY;

      Property Width:Single Read _Width;
      Property Height:Single Read _Height;
  End;

  BoidGene = Class(TERRAObject)
    Private
      _numberOfTrials : Single;
      _trialsWon :Single;

      _heading:Single;
      _speed:Single;
      _x:Single;
      _y:Single;
      _heightOfs:Single;
      _maximumSpeed:Single;
      _minimumSpeed:Single;
      _turnAngle:Single;
      _acceleration:Single;
      _AttackConstant:Single;
      _sensorRange:Single;
      _collisionRange:Single;
      _deathRange:Single;
      _deathConstant:Single;
      _retreatConstant:Single;

      _flock :BoidFlock;
      _TeamIndex : integer;

    Public

      constructor Create( direction,
                 magnitude,
                 xPos,
                 yPos,
                 heightOfs,
                 maxSpeed,
                 minSpeed,
                 angle,
                 accel,
                 sensor,
                 death,
                 deathC,
                 collision,
                 attackC,
                 retreatC : Single;
                 gaggle : BoidFlock;
                 TeamIndex : integer);

    function reproduceWith( o : BoidGene) : BoidGene;

    Function GetFitness(): Single;

    Property NumberOfTrials : Single Read _NumberOfTrials;
    Property TrialsWon : Single Read _TrialsWon;
    Property Acceleration: Single Read _Acceleration;
    Property CollisionRange: Single Read _CollisionRange;
    Property AttackConstant: Single Read _AttackConstant;
    Property DeathConstant: Single Read _DeathConstant;
    Property DeathRange: Single Read _DeathRange;
    Property Flock: BoidFlock Read _Flock;
    Property Heading: Single Read _Heading Write _Heading;
    Property MaximumSpeed: Single Read _MaximumSpeed;
    Property MinimumSpeed: Single Read _MinimumSpeed;
    Property RetreatConstant: Single Read _RetreatConstant;
    Property SensorRange: Single Read _SensorRange;
    Property Speed: Single Read _Speed Write _Speed;
    Property TurnAngle: Single Read _TurnAngle;
    Property X: Single Read _X Write _X;
    Property Y: Single Read _Y Write _Y;

      Property TeamIndex:Integer Read _TeamIndex;
  end;

  BoidAgent = Class(TERRAObject)
    Protected
      _x:Single;
      _y:Single;

      _teamIndex :integer;

      _heading:Single;
      _speed:Single;
      _heightOfs:Single;
      _maximumSpeed:Single;
      _minimumSpeed:Single;
      _turnAngle:Single;
      _acceleration:Single;
      _sensorRange:Single;
      _collisionRange:Single;
      _AttackConstant:Single;
      _deathRange:Single;
      _deathRangeNumber:Single;
      _deathConstant:Single;
      _retreatConstant : Single;

      _friendsHeading:Single;
      _friendsSpeed:Single;
      _friendsX:Single;
      _friendsY:Single;
      _friendsNumber : Single;

      _enemiesHeading:Single;
      _enemiesSpeed:Single;
      _enemiesX:Single;
      _enemiesY:Single;
      _enemiesNumber : Single;

      _flock : BoidFlock;
      _nearestFriend : BoidAgent; // Set only if within the collision range.
      _nearestEnemy : BoidAgent;

      _Instance:MeshInstance;

    Public

    { CONSTRUCTORS }
    constructor Create (direction, magnitude, xPos, yPos, heightOfs, maxSpeed, minSpeed,
                            angle, accel, sensor, death, deathC, attackC, retreatC,
                            collision : Single;
                            Color : ColorRGBA;
                            gaggle : BoidFlock); overload;

    constructor Create (b : BoidGene); overload;

    { Procedure }
    Procedure Release; override;

    { PROCEDURES }
    procedure move;
    procedure slowDown;
    procedure speedUp;
    procedure turnLeft;
    procedure turnRight;
    procedure senseFlock(f : BoidFlock);
    procedure decide;
    procedure avoidCollision;
    procedure attackEnemies;
    procedure hangOutWithFriends;
    procedure retreat;
    procedure turnAway(xPos, yPos: Single);
    procedure turnToward(xPos, yPos: Single);

    procedure setHeading( direction : Single);
    procedure setSpeed( magnitude : Single);
    procedure setTurnAngle( a : Single);

    { FUNCTIONS }
    function isInRange(b : BoidAgent; range : Single) : boolean;
    function isDead: boolean;
    function isFriend(b: BoidAgent): boolean;

    function relativeAngle(xPos, yPos : Single) : Single;

    Property Acceleration: Single Read _Acceleration;
    Property AttackConstant: Single Read _AttackConstant;
    Property CollisionRange: Single Read _CollisionRange;
    Property DeathConstant: Single Read _DeathConstant;
    Property DeathRange: Single Read _DeathRange;
    Property DeathRangeNumber: Single Read _DeathRangeNumber;
    Property EnemiesHeading: Single Read _EnemiesHeading;
    Property EnemiesNumber: Single Read _EnemiesNumber;
    Property EnemiesSpeed: Single Read _EnemiesSpeed;
    Property EnemiesX: Single Read _EnemiesX;
    Property EnemiesY: Single Read _EnemiesY;
    Property Flock: BoidFlock Read _Flock;
    Property FriendsHeading: Single Read _FriendsHeading;
    Property FriendsNumber: Single Read _FriendsNumber;
    Property FriendsSpeed: Single Read _FriendsSpeed;
    Property FriendsX: Single Read _FriendsX;
    Property FriendsY: Single Read _FriendsY;
    Property Heading: Single Read _Heading;
    Property MaximumSpeed: Single Read _MaximumSpeed;
    Property MinimumSpeed: Single Read _MinimumSpeed;
    Property NearestEnemy: BoidAgent Read _NearestEnemy;
    Property NearestFriend: BoidAgent Read _NearestFriend;
    Property RetreatConstant: Single Read _RetreatConstant;
    Property SensorRange: Single Read _SensorRange;
    Property Speed: Single Read _Speed Write SetSpeed;
    Property TurnAngle: Single Read _TurnAngle;
    Property X: Single Read _X;
    Property Y: Single Read _Y;
    Property TeamIndex:Integer Read _TeamIndex;
  End;

implementation
Uses TERRA_GraphicsManager, TERRA_Math, TERRA_Engine;

{ BoidAgent }
Constructor BoidAgent.Create(direction, magnitude, xPos, yPos, heightOfs, maxSpeed,
  minSpeed, angle, accel, sensor, death, deathC, attackC, retreatC,
  collision: Single; Color: ColorRGBA; gaggle: BoidFlock);
Begin
  _heightOfs := heightOfs;
  _X := xPos;
  _Y := yPos;

  _deathConstant := deathC;

  _MaximumSpeed := maxSpeed;
  _MinimumSpeed := minSpeed;
  _Acceleration := Accel;

    setHeading(direction);
    setSpeed(magnitude);
    setTurnAngle(angle);

  _SensorRange := sensor;
  _DeathRange := death;

  _attackConstant := attackC;
  _retreatConstant := retreatC;
  _collisionRange := collision;

  _flock := gaggle;
  _NearestFriend := nil;
  _NearestEnemy := nil;

  _TeamIndex := 0;
End;

//  Create a Boid as a sibling of another Boid
constructor BoidAgent.Create( b : BoidGene );
begin
  _X := b.X;
  _Y := b.Y;

  _deathConstant := b.DeathConstant;

  _MaximumSpeed := b.MaximumSpeed;
  _MinimumSpeed := b.MinimumSpeed;
  _Acceleration := b.Acceleration;
  
  setHeading(b.Heading);
  setSpeed(b.Speed);
  setTurnAngle(b.TurnAngle);
  
  _SensorRange := b.SensorRange;
  _DeathRange := b.DeathRange;

  _AttackConstant := b.AttackConstant;
  _RetreatConstant := b.RetreatConstant;
  _CollisionRange := b.CollisionRange;
  _Flock := b.Flock;

  _NearestFriend := nil;
  _NearestEnemy := nil;

  _teamIndex := b.teamIndex;
end;

Procedure BoidAgent.Release;
begin
  ReleaseObject(_Instance);

  inherited;
end;

//  Move the Boid                                                   
procedure BoidAgent.move;
begin
  _X := _X + Self.Speed * Cos(Self.Heading) * Engine.Graphics.ElapsedTime * BoidSpeed;
  _Y := _Y + Self.Speed * Sin(Self.Heading) * Engine.Graphics.ElapsedTime * BoidSpeed;
end;

// Slow Down (Never less than min speed)
Procedure BoidAgent.slowDown;
Begin
  SetSpeed(Self.Speed - Self.Acceleration);
End;

//  Speed Up (Never more than Max Speed)
procedure BoidAgent.speedUp;
begin
    setSpeed(Self.Speed + Self.Acceleration);
end;

//  Turn Left
procedure BoidAgent.turnLeft;
begin
  _Heading := Self.Heading + Self.TurnAngle;
end;

//  Turn Right                                                      
procedure BoidAgent.turnRight;
begin
  _Heading := Self.Heading - Self.TurnAngle;
end;

//  Is this Boid in range of a target Boid?                         
function BoidAgent.isInRange(b : BoidAgent; range : Single) : boolean;
begin
    Result := (sqrt((b.X-Self.X)*(b.X-Self.X) + (b.Y-Self.Y)*(b.Y-Self.Y)) < range);
end;

//  Check the whole flock for friends and enemies close by          
procedure BoidAgent.senseFlock(f : BoidFlock);
var b : BoidAgent;
    friends,
    totalFriendsX,
    totalFriendsY,
    totalFriendsSpeed,
    totalFriendsHeading,
    enemies,
    totalEnemiesX,
    totalEnemiesY,
    totalEnemiesSpeed,
    totalEnemiesHeading,
    distanceToFriend,
    distanceToEnemy : Single;
    i : integer;
begin
    friends := 0.0;
    totalFriendsX := 0.0;
    totalFriendsY := 0.0;
    totalFriendsSpeed := 0.0;
    totalFriendsHeading := 0.0;
    enemies := 0.0;
    totalEnemiesX := 0.0;
    totalEnemiesY := 0.0;
    totalEnemiesSpeed := 0.0;
    totalEnemiesHeading := 0.0;
    _NearestFriend := nil;
    _NearestEnemy := nil;
    _DeathRangeNumber := 0.0;
    distanceToFriend := Self.CollisionRange;
    distanceToEnemy := Self.SensorRange;

    for i := 0 to Pred(F._MemberCount) do
    begin
        b := F._Members[I];
        if (b <> self) then
        begin
            if (isFriend(b)) then
            begin
                if (isInRange(b, distanceToFriend)) then
                begin
                    _NearestFriend := b;
                    distanceToFriend :=
                        sqrt((b.X-X)*(b.X-X) +
                                   (b.Y-Y)*(b.Y-Y));
                end;
                {if(distanceToFriend < getCollisionRange) then // Not correct
                begin
                   turnAway(b.X,b.Y);
                end;}
                if (isInRange(b, Self.SensorRange)) then
                begin
                    friends := friends + 1.0;
                    totalFriendsX := totalFriendsX + b.X;
                    totalFriendsY := totalFriendsY + b.Y;
                    totalFriendsSpeed := totalFriendsSpeed + b.Speed;
                    totalFriendsHeading := totalFriendsHeading + b.Heading;
                end;
            end
            else
            begin // Is enemy
                if (isInRange(b, distanceToEnemy)) then
                begin
                    _NearestEnemy := b;
                    distanceToEnemy :=
                        sqrt((b.X-X)*(b.X-X) +
                                   (b.Y-Y)*(b.Y-Y));
                end;
                {if(b.getCollisionRange() < distanceToEnemy) then // Kinda defeats the point of hunting, but I cant think of another way to stop collisions
                begin
                  slowDown();
                end;}
                if (isInRange(b, Self.DeathRange)) then
                begin
                    _DeathRangeNumber := Self.DeathRangeNumber + 1.0;
                end;
                if (isInRange(b, Self.SensorRange)) then
                begin
                    enemies := enemies + 1.0;
                    totalEnemiesX := totalEnemiesX + b.X;
                    totalEnemiesY := totalEnemiesY + b.Y;
                    totalEnemiesSpeed := totalEnemiesSpeed + b.Speed;
                    totalEnemiesHeading := totalEnemiesHeading + b.Heading;
                end;
            end;
        end;

        if (friends = 0.0) then
        Begin
            _FriendsNumber := 0.0;
            _FriendsHeading := Heading;
            _FriendsSpeed := Speed;
            _FriendsX := X;
            _FriendsY := Y;
        End Else
        begin
            _FriendsNumber := friends;
            _FriendsHeading := totalFriendsHeading/friends;
            _FriendsSpeed := totalFriendsSpeed/friends;
            _FriendsX := totalFriendsX/friends;
            _FriendsY := totalFriendsY/friends;
        end;

        if (enemies = 0.0) then
        begin
            _EnemiesNumber := 0.0;
            _EnemiesHeading := Heading;
            _EnemiesSpeed := Speed;
            _EnemiesX := X;
            _EnemiesY := Y;
        End Else
        begin
            _EnemiesNumber := enemies;
            _EnemiesHeading := totalEnemiesHeading/enemies;
            _EnemiesSpeed := totalEnemiesSpeed/enemies;
            _EnemiesX := totalEnemiesX/enemies;
            _EnemiesY := totalEnemiesY/enemies;
        end;
    end;
end;

// This is where the boid decides what to do based on what it senses. 
procedure BoidAgent.decide;
begin
    if (Self.EnemiesNumber <> 0.0) then
    begin
      if ((Self.FriendsNumber+1)/Self.EnemiesNumber > Self.AttackConstant) then
      begin
          avoidCollision();
          attackEnemies();

      end
      else if ((Self.FriendsNumber+1)/Self.EnemiesNumber < Self.RetreatConstant) then
      begin
          retreat();
      end;
    end
    else if (NearestFriend <> nil) then
    begin
        avoidCollision();
    end
    else
    begin
        hangOutWithFriends();
    end;
end;

//  Try not to collide with others                                   
procedure BoidAgent.avoidCollision;
var n : BoidAgent;
  angle : Single;
begin
  n := NearestEnemy;
  if (n <> nil) then
  begin
      turnAway(n.X,n.Y);
      angle := relativeAngle(n.X,n.Y);
      if (Cos(n.Heading - self.Heading) > 0) then  // going the same direction
      begin
          if (cos(angle - Heading) > 0) then            // this boid is behind the other
          begin
              // Added to avoid collision Not working Properly Yet!!!!
              if(sqrt((n.X-X)*(n.X-X) + (n.Y-Y)*(n.Y-Y)) > n.collisionRange) then
              begin
                n.speedUp;
                slowDown;
                turnAway(n.X,n.Y);
              end;
              slowDown();
          end
          else
          begin                                                   // this boid is in front of the other
              speedUp();
          end;
      end
      else
      begin                                                   // going opposite directions
          if (Cos(angle - Heading) > 0) then            // head-on collision course
          begin
              slowDown();
          end;
      end;
  end;

  n := NearestFriend;
  if (n <> nil) then
  begin
      turnAway(n.X,n.Y);
      angle := relativeAngle(n.X,n.Y);
      if (Cos(n.Heading - self.Heading) > 0) then  // going the same direction
      begin
          if (cos(angle - Heading) > 0) then            // this boid is behind the other
          begin
              // Added to avoid collision Not working Properly yet!!!!
              if(sqrt((n.X-X)*(n.X-X) + (n.Y-Y)*(n.Y-Y)) > n.collisionRange) then
              begin
                //n.speedUp;
                slowDown;
                //turnAway(n.X,n.Y);
              end;
              slowDown();
          end
          else
          begin                                                   // this boid is in front of the other
              speedUp();
          end;
      end
      else
      begin                                                   // going opposite directions
          if (cos(angle - Heading) > 0) then            // head-on collision course
          begin
              slowDown();
          end;
      end;
  end;

end;

//  Stay with Friendly Boids                                         
procedure BoidAgent.hangOutWithFriends;
begin
    if (Self.FriendsNumber > 0.0) then
    begin
        turnToward(Self.FriendsX, Self.FriendsY);
        if (Self.FriendsSpeed > Speed) then
        begin
            speedUp();
        end
        else if (Self.FriendsSpeed < Speed) then
        begin
            slowDown();
        end;
    end
    else
    begin
        slowDown();
        if (RandomFloat(0.0, 1.0) < 0.5) then
        begin
            turnLeft();
        end
        else
        begin
            turnRight();
        end;
    end;
end;

//  Atack enemy boids to drive them away from the flock/school       
Procedure BoidAgent.attackEnemies;
Begin
    turnToward(Self.EnemiesX, Self.EnemiesY);

    If (Self.EnemiesSpeed > Speed) then
    Begin
        speedUp();
    End Else
    If (Self.EnemiesSpeed < Speed) then
    begin
        slowDown();
    end;
End;

//  Retreat from a fight                                             
procedure BoidAgent.retreat;
begin
    turnAway(Self.EnemiesX, Self.EnemiesY);
    speedUp();
end;

//  Get the relative angle from a Line                               
function BoidAgent.relativeAngle(xPos, yPos : Single) : Single;
var dx, dy, r, angle : Single;
begin
    dx := xPos - X;
    dy := yPos - Y;
    r := sqrt(dx*dx + dy*dy);
    angle := arccos(dx/r);
    if (dy < 0) then
    begin
        angle := 2.0*PI - angle;
    end;
    Result := angle;
end;

//  Turn away from a Specified Position
procedure BoidAgent.turnAway(xPos, yPos : Single);
var angle : Single;
begin
    if ((xPos = X) and (yPos = Y)) then
    begin
        exit;
    end;
    angle := relativeAngle(xPos,yPos);
    if (abs(angle-Heading) < PI)  then
    begin
        if (angle - Heading > 0) then
        begin
            turnRight();
        end
        else
        begin
            turnLeft();
        end;
    end
    else
    begin
        if (angle - Heading > 0) then
        begin
            turnLeft();
        end
        else
        begin
            turnRight();
        end;
    end;
end;

//  Turn Towards a Specified Position                                
procedure BoidAgent.turnToward(xPos, yPos : Single);
var angle : Single;
begin
    //Do seperation calculation here.
    if ((xPos = X) and (yPos = Y)) then
    begin
        turnAway(xPos,yPos);
        exit;
    end;
    angle := relativeAngle(xPos,yPos);
    if (abs(angle - Heading) < PI) then
    begin
        if (angle - Heading > 0) then
        begin
            turnLeft();
        end
        else
        begin
            turnRight();
        end;
    end
    else
    begin
        if (angle - Heading > 0) then
        begin
            turnRight();
        end
        else
        begin
            turnLeft();
        end;
    end;
end;

//  Examine boid colour to see if it is a friend                     
Function BoidAgent.isFriend(b : BoidAgent ) : boolean;
Begin
  Result := b.teamIndex = Self.teamIndex;
End;

//  Is the current boid dead (Not yet fully Functional)              
function BoidAgent.isDead : boolean;
begin
    Result := (RandomFloat(0.0, 1.0) < (1.0 - exp(-Self.DeathConstant*Self.DeathRangeNumber)));
    {*
     * If death constant is 0.231049, then there is a 50% chance
     * of dying when confronted with three enemies.
     *}
end;

procedure BoidAgent.setHeading( direction : Single);
begin
  _heading := direction;

  If (_heading > 2.0*PI) then
  Begin
    _heading := _heading - 2.0*PI;
  End Else
  Begin
    If (_heading < 0) then
    Begin
        _heading := 2.0*PI + _heading;
    End;
  End;
End;

Procedure BoidAgent.setSpeed( magnitude : Single);
Begin
  _speed := abs(magnitude);
  if (_speed > Self.MaximumSpeed) then
    _speed := Self.MaximumSpeed;

  if (_speed < Self.MinimumSpeed) then
    _speed := Self.MinimumSpeed;
End;


procedure BoidAgent.setTurnAngle( a : Single);
begin
    while (a>2.0*PI) do
    begin
        a := a - 2.0*PI;
    end;
    while (a<0) do
    begin
        a := 2.0*PI + a;
    end;

    _turnAngle := a;
end;

{ BoidGene }
//  Basic data class (Gene Memory) of a boid,
//  Will pass this on to any Children of the parent boid
constructor BoidGene.Create(direction, magnitude, xPos, yPos,heightOfs, maxSpeed,
  minSpeed, angle, accel, sensor, death, deathC, collision, attackC,
  retreatC: Single; gaggle: BoidFlock; TeamIndex : integer);
begin
  _heightOfs := heightOfs;
  _MaximumSpeed := maxSpeed;
  _MinimumSpeed := minSpeed;

  Self.Heading := direction;
  Self.Speed := magnitude;

  _X := xPos;
  _Y := yPos;

  _TurnAngle := angle;
  _Acceleration := accel;
  _SensorRange := sensor;
  _DeathRange := death;
  _DeathConstant := deathC;
  _AttackConstant := attackC;
  _RetreatConstant := attackC;
  _CollisionRange := collision;
  _Flock := Gaggle;

  _NumberOfTrials := 0.0;
  _TrialsWon := 0.0;

  _TeamIndex := TeamIndex;
end;

function BoidGene.getFitness: Single;
begin
  Result := TrialsWon/NumberOfTrials;
end;

//  Reproduction takes place
function BoidGene.reproduceWith(o : BoidGene): BoidGene;
Var
  direction, magnitude, xPos, yPos, maxSpeed, minSpeed, turnRate,
  accel, sRange, cRange, attackC, retreatC : Single;
Begin
     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         direction := o.Heading;
     end
     else
     begin
         direction := Self.Heading;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         magnitude := o.Speed;
     end
     else
     begin
         magnitude := Self.Speed;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         xPos := o.X;
     end
     else
     begin
         xPos := Self.X;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         yPos := o.Y;
     end
     else
     begin
         yPos := Self.Y;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         maxSpeed := o.MaximumSpeed;
     end
     else
     begin
         maxSpeed := Self.MaximumSpeed;
     end;
     
     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         minSpeed := o.MinimumSpeed;
     end
     else
     begin
         minSpeed := Self.MinimumSpeed;
     end;
     if (minSpeed > maxSpeed) then
     begin
         maxSpeed := minSpeed;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         turnRate := o.TurnAngle;
     end
     else
     begin
         turnRate := Self.TurnAngle;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         accel := o.Acceleration;
     end
     else
     begin
         accel := Self.Acceleration;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         sRange := o.SensorRange;
     end
     else
     begin
         sRange := Self.SensorRange;
     end;
     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         cRange := o.CollisionRange;
     end
     else
     begin
         cRange := Self.CollisionRange;
     end;
     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         attackC := o.AttackConstant;
     end
     else
     begin
         attackC := Self.AttackConstant;
     end;

     if (RandomFloat(0.0, 1.0) > 0.5) then
     begin
         retreatC := o.RetreatConstant;
     end
     else
     begin
         retreatC := Self.RetreatConstant;
     end;

     Result := BoidGene.Create(
                     direction,
                     magnitude,
                     xPos,
                     yPos,
                     _heightOfs,
                     maxSpeed,
                     minSpeed,
                     turnRate,
                     accel,
                     sRange,
                     DeathRange,
                     DeathConstant,
                     cRange,
                     attackC,
                     retreatC,
                     Flock,
                     teamIndex);

end;

{ BoidFlock }
//  Holding Flock for all boids in our aquarium
procedure BoidFlock.add(b: BoidAgent);
begin
  Inc(_MemberCount);
  SetLength(_members, _MemberCount);
  _members[Pred(_MemberCount)] := b;
end;

Procedure BoidFlock.addToMomentum(b: BoidAgent);
Var
  x, y, r, angle : Single;
Begin
  x := MeanSpeed *cos(MeanHeading) + b.Speed*cos(b.Heading);
  y := MeanSpeed *sin(MeanHeading) + b.Speed*sin(b.Heading);
  r := sqrt(x*x + y*y);
  angle := arccos(x/r);
  if (y < 0) then
  begin
      angle := 2.0*PI - angle;
  end;

  MeanSpeed := r;
  MeanHeading := Angle;
End;

function BoidFlock.calculateMeanX: Single;
var
  sum : Single;
  i : integer;
begin
  sum := 0;
  for i := 0 to Pred(_MemberCount) do
  begin
    sum := sum + _Members[i].X;
  end;

  Result := sum/_MemberCount;
End;

Function BoidFlock.calculateMeanY: Single;
Var
  sum : Single;
  i : integer;
Begin
  sum := 0;
  for i := 0 to Pred(_MemberCount) do
  begin
    sum := sum + _Members[i].Y;
  end;
  Result := sum/_MemberCount;
End;

//  Wrap around when Boid goes out of bounds                         
procedure BoidFlock.checkBounds(boid: BoidAgent);
begin
  if (boid.X > _width) then
  begin
      boid._X := boid.X - _width;
  end;
  if (boid.X < 0) then
  begin
      boid._X := boid.X + _width;
  end;
  if (boid.Y > _height) then
  begin
      boid._Y := boid.Y - _height;
  end;
  if (boid.Y < 0) then
  begin
      boid._Y := boid.Y + _height;
  end;
end;

//  Create the flock                                                 
Constructor BoidFlock.Create(size: integer; h, w: Single);
Var
  i :integer;
Begin
  Self._TeamCount := 0;
  Self._MemberCount := 0;
  _Height := h;
  _Width := w;
End;

procedure BoidFlock.initializeDuel(gene, challenger: BoidGene);
var i : integer;
begin
{  members := TObjectList.Create;
  for i := 0 to getNumberOfMembers -1 do
  begin
      add(BoidAgent.Create(gene));
  end;
  for i := 0 to getNumberOfMembers -1 do
  begin
      add(BoidAgent.Create(challenger));
  end;
  setMeanX(calculateMeanX());
  setMeanY(calculateMeanY()); }
end;

//  Calculate next movement for all boids                            
Procedure BoidFlock.Update;
Var
  boid:BoidAgent;
  teamSeen:Integer;
  i:Integer;
Begin
  teamSeen := -1;
  MeanHeading := 0.0;
  MeanSpeed := 0.0;
  MeanX := 0.0;
  MeanY := 0.0;

  For i := 0 to Pred(_MemberCount) do
  Begin
      boid := _Members[i];
      if (_TeamCount > 1) then
      begin
          if (teamSeen <0) then
          begin
              teamSeen := boid.teamIndex;
          end;
      end;
      boid.senseFlock(self);
      boid.decide();
      boid.move();
      checkBounds(boid);
      addToMomentum(boid);
      MeanX := MeanX + boid.X;
      MeanY := MeanY + boid.Y;
  end;

  MeanX := MeanX/MemberCount;
  MeanY := MeanY/MemberCount;
end;

function BoidFlock.FindAngle( FSourceX, FSourceY, FDestX, FDestY  : single) : single;
var
  XDiff, YDiff: Single;
  fpAngle: Single;
  SourcePoint , DestPoint :Vector2D;
  N:Vector2D;
begin
{  FDestX := FSourceX - 50;
  FDestY := FSourceY;}

  SourcePoint := Vector2D_Create(FSourceX, FSourceY);
  DestPoint   := Vector2D_Create(FDestX, FDestY);

  N := DestPoint;
  N.Subtract(SourcePoint);
  N.Normalize();

  fpAngle := Atan2(-N.Y, N.X);
  Result := fpAngle + 90*RAD;
end;

//  Render the flock                                                 
Procedure BoidFlock.Render(View:TERRAViewport);
var i : integer;
    tempBoid : BoidAgent;
    xPoints : array [0..2] of single;
    yPoints : array [0..2] of single;
    rotangle : Single;
    LocalX, LocalY,PositionColor : single;
begin
  Self.Update();

  for i := 0 to Pred(_MemberCount) do
  begin
    tempBoid := _Members[i];

    If tempBoid = Nil Then
      Continue;

    If tempBoid._Instance = Nil Then
    Begin
      tempBoid._Instance := MeshInstance.Create(_Teams[tempBoid.TeamIndex]);
      tempBoid._Instance.Animation.Play(tempBoid._Instance.Animation.Find('idle'));
    End;

    xPoints[0] := (tempBoid.X+20.0*cos(tempBoid.Heading));
    yPoints[0] := (tempBoid.Y+20.0*sin(tempBoid.Heading));
    xPoints[1] := (tempBoid.X+5.0*cos(tempBoid.Heading + (2.0/3.0)*PI));
    yPoints[1] := (tempBoid.Y+5.0*sin(tempBoid.Heading + (2.0/3.0)*PI));
    xPoints[2] := (tempBoid.X+5.0*cos(tempBoid.Heading - (2.0/3.0)*PI));
    yPoints[2] := (tempBoid.Y+5.0*sin(tempBoid.Heading - (2.0/3.0)*PI));


    LocalX := tempBoid.X;
    LocalY := tempBoid.Y;

    tempBoid._Instance.SetPosition(Vector3D_Create(LocalX, tempBoid._heightOfs, LocalY));

    rotangle := FindAngle(LocalX, LocalY, xPoints[0], yPoints[0]);

    tempBoid._Instance.SetRotation(Vector3D_Create(0, rotAngle, 0));

    Engine.Graphics.AddRenderable(View, tempBoid._Instance);

    {if ShowLines then
    begin
      glBegin(GL_LINES);
        glDisable(GL_TEXTURE_2D);
        glColor3f(1.0,1.0,1.0);
        glVertex3f(tempBoid.x/100,tempBoid.y/100,0.0);
        glVertex3f(tempBoid.friendsX/100,tempBoid.friendsY/100,0.0);
      glEnd;
    end;
    if ShowEnemyLines then
    begin
      glBegin(GL_LINES);
        glColor3f(0.0,0.0,0.0);
        glVertex3f(tempBoid.x/100,tempBoid.y/100,0.0);
        glVertex3f(tempBoid.enemiesX/100,tempBoid.enemiesY/100,0.0);
      glEnd;
    end;}
  End;
End;

procedure BoidFlock.remove(b: BoidAgent);
Var
  I:Integer;
begin
  I := 0;
  While I<_MemberCount Do
  If (_members[I] = B) Then
  Begin
    _members[I] := _members[Pred(_MemberCount)];
    Dec(_MemberCount);
    ReleaseObject(B);
  End Else
    Inc(I);
end;

//  Procedure                                                         
Procedure BoidFlock.Release;
Var
  I:Integer;
Begin
  For I:=0 To Pred(_MemberCount) Do
    ReleaseObject(_Members[I]);
End;

procedure BoidFlock.addBoid(direction, magnitude, xPos, yPos, heightOfs, maxSpeed,
  minSpeed, angle, accel, sensor, death, deathC, collision, attackC,
  retreatC: Single; TeamIndex: integer);
Var
  gene:BoidGene;
Begin
    gene := BoidGene.Create(
                     2*RandomFloat(0.0, 1.0)*PI,                   // heading
                     RandomFloat(0.0, 1.0),                     // speed
                     RandomFloat(0.0, 1.0)*_width,                  // x position
                     RandomFloat(0.0, 1.0)*_height,                 // y position
                     heightOfs,
                     maxSpeed,                      // maximum speed
                     minSpeed,                      // minimum speed
                     angle,                         // turning rate
                     accel,                         // acceleration
                     sensor,                        // sensor range
                     death,                         // death range
                     deathC,                        // death constant
                     collision,                     // collision range
                     attackC,                       // attack constant
                     retreatC,                      // retreat constant
                     self,                          // This Boid
                     TeamIndex);                   // flock

    add(BoidAgent.Create(gene));

    ReleaseObject(Gene);
end;


procedure BoidFlock.AddTeam(Mesh:TERRAMesh);
begin
  If Mesh = Nil Then
    Exit;

  Inc(_TeamCount);
  SetLength(_teams, _TeamCount);
  _teams[Pred(_TeamCount)] := Mesh;
end;

end.


