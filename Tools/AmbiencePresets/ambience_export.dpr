program ExportAmb;

{$APPTYPE CONSOLE}

uses
  SysUtils, TERRA_Object, TERRA_XML, TERRA_AudioReverb, TERRA_Vector3D, TERRA_SoundAmbience, TERRA_Math, TERRA_String;

Function eaxDbToAmp(Const eaxDb:Single):Single;
Begin
  Result := Power(10.0,  eaxDb / 2000.0);
End;

Procedure ExportPreset(Const Name:TERRAString; Const environment:Integer; Const environmentSize, environmentDiffusion:Single; Const room, roomHF, roomLF:Integer;
                        Const decayTime, decayHFRatio, decayLFRatio:Single;
                        Const reflections:Integer; Const reflectionsDelay:Single; Const reflectionsPan:Vector3D;
                        Const reverb:Integer; Const reverbDelay:Single; Const reverbPan:Vector3D;
                        Const echoTime, echoDepth, modulationTime, modulationDepth, airAbsorptionHF:Single;
                        Const hfReference, lfReference, roomRolloffFactor:Single);
Var
  Amb:SoundAmbience;
  XML:XMLNode;
Begin
  Amb := SoundAmbience.Create(Name);
  Amb.Density := 1.0; // todo, currently default
  Amb.Diffusion := EnvironmentDiffusion;
  Amb.Gain :=  eaxDbToAmp(room); //0.32f;
  Amb.GainHF := eaxDbToAmp(roomHF); //0.89f;
  Amb.GainLF := eaxDbToAmp(roomLF); // 1.0f;
  Amb.DecayTime := decayTime;
  Amb.DecayHFRatio := decayHFRatio;
  Amb.DecayLFRatio := decayLFRatio;
  Amb.ReflectionsGain := eaxDbToAmp(reflections); // 0.05f;
  Amb.ReflectionsDelay := reflectionsDelay;
  Amb.ReflectionsPan := reflectionsPan;
  Amb.LateReverbGain := eaxDbToAmp(reverb); //1.26f;
  Amb.LateReverbDelay := reverbDelay;
  Amb.LateReverbPan := reverbPan;
  Amb.EchoTime := echoTime;
  Amb.EchoDepth := echoDepth;
  Amb.ModulationTime := modulationTime;
  Amb.ModulationDepth := modulationDepth;
  Amb.AirAbsorptionGainHF := eaxDbToAmp(airAbsorptionHF); //0.995f;
  Amb.HFReference := hfReference;
  Amb.LFReference := lfReference;
  Amb.RoomRolloffFactor := roomRolloffFactor;
  Amb.DecayHFLimit := True;

  XML := XMLNode.Create();
  XML.LoadFromObject(Amb);
  XML.SaveToFile('amb_'+Name+'.xml');

  ReleaseObject(Amb);
End;

begin
  // CASTLE PRESETS
  ExportPreset('CastleSmallRoom', 26, 8.3, 0.890, -1000, -800, -2000, 1.22, 0.83, 0.31, -100, 0.022, Vector3D_Zero, 600, 0.011, Vector3D_Zero, 0.138, 0.080, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleShortPassage', 26, 8.3, 0.890, -1000, -1000, -2000, 2.32, 0.83, 0.31, -100, 0.007, Vector3D_Zero, 200, 0.023, Vector3D_Zero, 0.138, 0.080, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleMediumroom', 26, 8.3, 0.930, -1000, -1100, -2000, 2.04, 0.83, 0.46, -400, 0.022, Vector3D_Zero, 400, 0.011, Vector3D_Zero, 0.155, 0.030, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleLongpassage', 26, 8.3, 0.890, -1000, -800, -2000, 3.42, 0.83, 0.31, -100, 0.007, Vector3D_Zero, 300, 0.023, Vector3D_Zero, 0.138, 0.080, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleLargeroom', 26, 8.3, 0.820, -1000, -1100, -1800, 2.53, 0.83, 0.50, -700, 0.034, Vector3D_Zero, 200, 0.016, Vector3D_Zero, 0.185, 0.070, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleHall', 26, 8.3, 0.810, -1000, -1100, -1500, 3.14, 0.79, 0.62, -1500, 0.056, Vector3D_Zero, 100, 0.024, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleCupboard', 26, 8.3, 0.890, -1000, -1100, -2000, 0.67, 0.87, 0.31, 300, 0.010, Vector3D_Zero, 1100, 0.007, Vector3D_Zero, 0.138, 0.080, 0.250, 0, -5, 5168.6, 139.5, 0);
  ExportPreset('CastleCourtyard', 26, 8.3, 0.420, -1000, -700, -1400, 2.13, 0.61, 0.23, -1300, 0.160, Vector3D_Zero, -300, 0.036, Vector3D_Zero, 0.250, 0.370, 0.250, 0, -5, 5000, 250, 0); //0x1
  ExportPreset('CastleAlcove', 26, 8.3, 0.890, -1000, -600, -2000, 1.64, 0.87, 0.31, 00, 0.007, Vector3D_Zero, 300, 0.034, Vector3D_Zero, 0.138, 0.080, 0.250, 0, -5, 5168.6, 139.5, 0);

  // FACTORY PRESETS
  ExportPreset('FactoryAlcove', 26, 1.8, 0.590, -1200, -200, -600, 3.14, 0.65, 1.31, 300, 0.010, Vector3D_Zero, 000, 0.038, Vector3D_Zero, 0.114, 0.100, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryShortpassage', 26, 1.8, 0.640, -1200, -200, -600, 2.53, 0.65, 1.31, 0, 0.010, Vector3D_Zero, 200, 0.038, Vector3D_Zero, 0.135, 0.230, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryMediumroom', 26, 1.9, 0.820, -1200, -200, -600, 2.76, 0.65, 1.31, -1100, 0.022, Vector3D_Zero, 300, 0.023, Vector3D_Zero, 0.174, 0.070, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryLongpassage', 26, 1.8, 0.640, -1200, -200, -600, 4.06, 0.65, 1.31, 0, 0.020, Vector3D_Zero, 200, 0.037, Vector3D_Zero, 0.135, 0.230, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryLargeroom', 26, 1.9, 0.750, -1200, -300, -400, 4.24, 0.51, 1.31, -1500, 0.039, Vector3D_Zero, 100, 0.023, Vector3D_Zero, 0.231, 0.070, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryHall', 26, 1.9, 0.750, -1000, -300, -400, 7.43, 0.51, 1.31, -2400, 0.073, Vector3D_Zero, -100, 0.027, Vector3D_Zero, 0.250, 0.070, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryCupboard', 26, 1.7, 0.630, -1200, -200, -600, 0.49, 0.65, 1.31, 200, 0.010, Vector3D_Zero, 600, 0.032, Vector3D_Zero, 0.107, 0.070, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactoryCourtyard', 26, 1.7, 0.570, -1000, -1000, -400, 2.32, 0.29, 0.56, -1300, 0.140, Vector3D_Zero, -800, 0.039, Vector3D_Zero, 0.250, 0.290, 0.250, 0, -5, 3762.6, 362.5, 0);
  ExportPreset('FactorySmallroom', 26, 1.8, 0.820, -1000, -200, -600, 1.72, 0.65, 1.31, -300, 0.010, Vector3D_Zero, 500, 0.024, Vector3D_Zero, 0.119, 0.070, 0.250, 0, -5, 3762.6, 362.5, 0);

  // ICE PALACE PRESETS
  ExportPreset('IcepalaceAlcove', 26, 2.7, 0.840, -1000, -500, -1100, 2.76, 1.46, 0.28, 100, 0.010, Vector3D_Zero, -100, 0.030, Vector3D_Zero, 0.161, 0.090, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceShortpassage', 26, 2.7, 0.750, -1000, -500, -1100, 1.79, 1.46, 0.28, -600, 0.010, Vector3D_Zero, 100, 0.019, Vector3D_Zero, 0.177, 0.090, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceMediumroom', 26, 2.7, 0.870, -1000, -500, -700, 2.22, 1.53, 0.32, -800, 0.039, Vector3D_Zero, 100, 0.027, Vector3D_Zero, 0.186, 0.120, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceLongpassage', 26, 2.7, 0.770, -1000, -500, -800, 3.01, 1.46, 0.28, -200, 0.012, Vector3D_Zero, 200, 0.025, Vector3D_Zero, 0.186, 0.040, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceLargeroom', 26, 2.9, 0.810, -1000, -500, -700, 3.14, 1.53, 0.32, -1200, 0.039, Vector3D_Zero, 000, 0.027, Vector3D_Zero, 0.214, 0.110, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceHall', 26, 2.9, 0.760, -1000, -700, -500, 5.49, 1.53, 0.38, -1900, 0.054, Vector3D_Zero, -400, 0.052, Vector3D_Zero, 0.226, 0.110, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceCupboard', 26, 2.7, 0.830, -1000, -600, -1300, 0.76, 1.53, 0.26, 100, 0.012, Vector3D_Zero, 600, 0.016, Vector3D_Zero, 0.143, 0.080, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceCourtyard', 26, 2.9, 0.590, -1000, -1100, -1000, 2.04, 1.20, 0.38, -1000, 0.173, Vector3D_Zero, -1000, 0.043, Vector3D_Zero, 0.235, 0.480, 0.250, 0, -5, 12428.5, 99.6, 0);
  ExportPreset('IcepalaceSmallroom', 26, 2.7, 0.840, -1000, -500, -1100, 1.51, 1.53, 0.27, -100, 0.010, Vector3D_Zero, 300, 0.011, Vector3D_Zero, 0.164, 0.140, 0.250, 0, -5, 12428.5, 99.6, 0);

  // SPACE STATION PRESETS
  ExportPreset('SpacestationAlcove', 26, 1.5, 0.780, -1000, -300, -100, 1.16, 0.81, 0.55, 300, 0.007, Vector3D_Zero, 000, 0.018, Vector3D_Zero, 0.192, 0.210, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationMediumroom', 26, 1.5, 0.750, -1000, -400, -100, 3.01, 0.50, 0.55, -800, 0.034, Vector3D_Zero, 100, 0.035, Vector3D_Zero, 0.209, 0.310, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationShortpassage', 26, 1.5, 0.870, -1000, -400, -100, 3.57, 0.50, 0.55, 0, 0.012, Vector3D_Zero, 100, 0.016, Vector3D_Zero, 0.172, 0.200, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationLongpassage', 26, 1.9, 0.820, -1000, -400, -100, 4.62, 0.62, 0.55, 0, 0.012, Vector3D_Zero, 200, 0.031, Vector3D_Zero, 0.250, 0.230, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationLargeroom', 26, 1.8, 0.810, -1000, -400, -100, 3.89, 0.38, 0.61, -1000, 0.056, Vector3D_Zero, -100, 0.035, Vector3D_Zero, 0.233, 0.280, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationHall', 26, 1.9, 0.870, -1000, -400, -100, 7.11, 0.38, 0.61, -1500, 0.100, Vector3D_Zero, -400, 0.047, Vector3D_Zero, 0.250, 0.250, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationCupboard', 26, 1.4, 0.560, -1000, -300, -100, 0.79, 0.81, 0.55, 300, 0.007, Vector3D_Zero, 500, 0.018, Vector3D_Zero, 0.181, 0.310, 0.250, 0, -5, 3316.1, 458.2, 0);
  ExportPreset('SpacestationSmallroom', 26, 1.5, 0.700, -1000, -300, -100, 1.72, 0.82, 0.55, -200, 0.007, Vector3D_Zero, 300, 0.013, Vector3D_Zero, 0.188, 0.260, 0.250, 0, -5, 3316.1, 458.2, 0);

  // WOODEN GALLEON PRESETS
  ExportPreset('WoodenAlcove', 26, 7.5, 1, -1000, -1800, -1000, 1.22, 0.62, 0.91, 100, 0.012, Vector3D_Zero, -300, 0.024, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0); // 0x3;
  ExportPreset('WoodenShortpassage', 26, 7.5, 1, -1000, -1800, -1000, 1.75, 0.50, 0.87, -100, 0.012, Vector3D_Zero, -400, 0.024, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenMediumroom', 26, 7.5, 1, -1000, -2000, -1100, 1.47, 0.42, 0.82, -100, 0.049, Vector3D_Zero, -100, 0.029, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenLongpassage', 26, 7.5, 1, -1000, -2000, -1000, 1.99, 0.40, 0.79, 000, 0.020, Vector3D_Zero, -700, 0.036, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenLargeroom', 26, 7.5, 1, -1000, -2100, -1100, 2.65, 0.33, 0.82, -100, 0.066, Vector3D_Zero, -200, 0.049, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenHall', 26, 7.5, 1, -1000, -2200, -1100, 3.45, 0.30, 0.82, -100, 0.088, Vector3D_Zero, -200, 0.063, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenCupboard', 26, 7.5, 1, -1000, -1700, -1000, 0.56, 0.46, 0.91, 100, 0.012, Vector3D_Zero, 100, 0.028, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenSmallroom', 26, 7.5, 1, -1000, -1900, -1000, 0.79, 0.32, 0.87, 00, 0.032, Vector3D_Zero, -100, 0.029, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);
  ExportPreset('WoodenCourtyard', 26, 7.5, 0.650, -1000, -2200, -1000, 1.79, 0.35, 0.79, -500, 0.123, Vector3D_Zero, -2000, 0.032, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 4705, 99.6, 0);

  // SPORTS PRESETS
  ExportPreset('SportEmptystadium', 26, 7.2, 1, -1000, -700, -200, 6.26, 0.51, 1.10, -2400, 0.183, Vector3D_Zero, -800, 0.038, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('SportSquashcourt', 26, 7.5, 0.750, -1000, -1000, -200, 2.22, 0.91, 1.16, -700, 0.007, Vector3D_Zero, -200, 0.011, Vector3D_Zero, 0.126, 0.190, 0.250, 0, -5, 7176.9, 211.2, 0);
  ExportPreset('SportSmallswimmingpool', 26, 36.2, 0.700, -1000, -200, -100, 2.76, 1.25, 1.14, -400, 0.020, Vector3D_Zero, -200, 0.030, Vector3D_Zero, 0.179, 0.150, 0.895, 0.190, -5, 5000, 250, 0);
  ExportPreset('SportLargeswimmingpool', 26, 36.2, 0.820, -1000, -200, 0, 5.49, 1.31, 1.14, -700, 0.039, Vector3D_Zero, -600, 0.049, Vector3D_Zero, 0.222, 0.550, 1.159, 0.210, -5, 5000, 250, 0);
  ExportPreset('SportGymnasium', 26, 7.5, 0.810, -1000, -700, -100, 3.14, 1.06, 1.35, -800, 0.029, Vector3D_Zero, -500, 0.045, Vector3D_Zero, 0.146, 0.140, 0.250, 0, -5, 7176.9, 211.2, 0);
  ExportPreset('SportFullstadium', 26, 7.2, 1, -1000, -2300, -200, 5.25, 0.17, 0.80, -2000, 0.188, Vector3D_Zero, -1100, 0.038, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('SportStadimtannoy', 26, 3, 0.780, -1000, -500, -600, 2.53, 0.88, 0.68, -1100, 0.230, Vector3D_Zero, -600, 0.063, Vector3D_Zero, 0.250, 0.200, 0.250, 0, -5, 5000, 250, 0);

  // PREFAB PRESETS
  ExportPreset('PrefabWorkshop', 26, 1.9, 1, -1000, -1700, -800, 0.76, 1, 1, 0, 0.012, Vector3D_Zero, 100, 0.012, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('PrefabSchoolroom', 26, 1.86, 0.690, -1000, -400, -600, 0.98, 0.45, 0.18, 300, 0.017, Vector3D_Zero, 300, 0.015, Vector3D_Zero, 0.095, 0.140, 0.250, 0, -5, 7176.9, 211.2, 0);
  ExportPreset('PrefabPractiseroom', 26, 1.86, 0.870, -1000, -800, -600, 1.12, 0.56, 0.18, 200, 0.010, Vector3D_Zero, 300, 0.011, Vector3D_Zero, 0.095, 0.140, 0.250, 0, -5, 7176.9, 211.2, 0);
  ExportPreset('PrefabOuthouse', 26, 80.3, 0.820, -1000, -1900, -1600, 1.38, 0.38, 0.35, -100, 0.024, Vector3D_Zero, -400, 0.044, Vector3D_Zero, 0.121, 0.170, 0.250, 0, -5, 2854.4, 107.5, 0);
  ExportPreset('PrefabCaravan', 26, 8.3, 1, -1000, -2100, -1800, 0.43, 1.50, 1, 0, 0.012, Vector3D_Zero, 600, 0.012, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0); //0x1

  // DOME AND PIPE PRESETS
  ExportPreset('DomeTomb', 26, 51.8, 0.790, -1000, -900, -1300, 4.18, 0.21, 0.10, -825, 0.030, Vector3D_Zero, 450, 0.022, Vector3D_Zero, 0.177, 0.190, 0.250, 0, -5, 2854.4, 20, 0);
  ExportPreset('DomeSaintPauls', 26, 50.3, 0.870, -1000, -900, -1300, 10.48, 0.19, 0.10, -1500, 0.090, Vector3D_Zero, 200, 0.042, Vector3D_Zero, 0.250, 0.120, 0.250, 0, -5, 2854.4, 20, 0); //0x3);
  ExportPreset('PipeSmall', 26, 50.3, 1, -1000, -900, -1300, 5.04, 0.10, 0.10, -600, 0.032, Vector3D_Zero, 800, 0.015, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 2854.4, 20, 0); // 0x3);
  ExportPreset('PipeLongthin', 26, 1.6, 0.910, -1000, -700, -1100, 9.21, 0.18, 0.10, -300, 0.010, Vector3D_Zero, -300, 0.022, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 2854.4, 20, 0);
  ExportPreset('PipeLarge', 26, 50.3, 1, -1000, -900, -1300, 8.45, 0.10, 0.10, -800, 0.046, Vector3D_Zero, 400, 0.032, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 2854.4, 20, 0);
  ExportPreset('PipeResonant', 26, 1.3, 0.910, -1000, -700, -1100, 6.81, 0.18, 0.10, -300, 0.010, Vector3D_Zero, 00, 0.022, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 2854.4, 20, 0);

  // OUTDOORS PRESETS
  ExportPreset('OutdoorsBackyard', 26, 80.3, 0.450, -1000, -1200, -600, 1.12, 0.34, 0.46, -700, 0.069, Vector3D_Zero, -300, 0.023, Vector3D_Zero, 0.218, 0.340, 0.250, 0, -5, 4399.1, 242.9, 0);
  ExportPreset('OutdoorsRollingplains', 26, 80.3, 0, -1000, -3900, -400, 2.13, 0.21, 0.46, -1500, 0.300, Vector3D_Zero, -700, 0.019, Vector3D_Zero, 0.250, 1, 0.250, 0, -5, 4399.1, 242.9, 0);
  ExportPreset('OutdoorsDeepcanyon', 26, 80.3, 0.740, -1000, -1500, -400, 3.89, 0.21, 0.46, -1000, 0.223, Vector3D_Zero, -900, 0.019, Vector3D_Zero, 0.250, 1, 0.250, 0, -5, 4399.1, 242.9, 0);
  ExportPreset('OutdoorsCreek', 26, 80.3, 0.350, -1000, -1500, -600, 2.13, 0.21, 0.46, -800, 0.115, Vector3D_Zero, -1400, 0.031, Vector3D_Zero, 0.218, 0.340, 0.250, 0, -5, 4399.1, 242.9, 0);
  ExportPreset('OutdoorsValley', 26, 80.3, 0.280, -1000, -3100, -1600, 2.88, 0.26, 0.35, -1700, 0.263, Vector3D_Zero, -800, 0.100, Vector3D_Zero, 0.250, 0.340, 0.250, 0, -5, 2854.4, 107.5, 0);

  // MOOD PRESETS
  ExportPreset('MoodHeaven', 26, 19.6, 0.940, -1000, -200, -700, 5.04, 1.12, 0.56, -1230, 0.020, Vector3D_Zero, 200, 0.029, Vector3D_Zero, 0.250, 0.080, 2.742, 0.050, -2, 5000, 250, 0);
  ExportPreset('MoodHell', 26, 100, 0.570, -1000, -900, -700, 3.57, 0.49, 2, -10000, 0.020, Vector3D_Zero, 300, 0.030, Vector3D_Zero, 0.110, 0.040, 2.109, 0.520, -5, 5000, 139.5, 0); //, 0x40);
  ExportPreset('MoodMemory', 26, 8, 0.850, -1000, -400, -900, 4.06, 0.82, 0.56, -2800, 0, Vector3D_Zero, 100, 0, Vector3D_Zero, 0.250, 0, 0.474, 0.450, -10, 5000, 250, 0);

  // DRIVING SIMULATION PRESETS
  ExportPreset('DrivingCommentator', 26, 3, 0, 1000, -500, -600, 2.42, 0.88, 0.68, -1400, 0.093, Vector3D_Zero, -1200, 0.017, Vector3D_Zero, 0.250, 1, 0.250, 0, -10, 5000, 250, 0);
  ExportPreset('DrivingPitgarage', 26, 1.9, 0.590, -1000, -300, -500, 1.72, 0.93, 0.87, -500, 0, Vector3D_Zero, 200, 0.016, Vector3D_Zero, 0.250, 0.110, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('DrivingIncarRacer', 26, 1.1, 0.800, -1000, 0, -200, 0.17, 2, 0.41, 500, 0.007, Vector3D_Zero, -300, 0.015, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 10268.2, 251, 0);
  ExportPreset('DrivingIncarSports', 26, 1.1, 0.800, -1000, -400, 0, 0.17, 0.75, 0.41, 0, 0.010, Vector3D_Zero, -500, 0, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 10268.2, 251, 0);
  ExportPreset('DrivingIncarLuxury', 26, 1.6, 1, -1000, -2000, -600, 0.13, 0.41, 0.46, -200, 0.010, Vector3D_Zero, 400, 0.010, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 10268.2, 251, 0);
  ExportPreset('DrivingFullgrandstand', 26, 8.3, 1, -1000, -1100, -400, 3.01, 1.37, 1.28, -900, 0.090, Vector3D_Zero, -1500, 0.049, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 10420.2, 250, 0); //, 0x1);
  ExportPreset('DrivingEmptygrandstand', 26, 8.3, 1, -1000, 0, -200, 4.62, 1.75, 1.40, -1363, 0.090, Vector3D_Zero, -1200, 0.049, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 10420.2, 250, 0); //, 0x1);
  ExportPreset('DrivingTunnel', 26, 3.1, 0.810, -1000, -800, -100, 3.42, 0.94, 1.31, -300, 0.051, Vector3D_Zero, -300, 0.047, Vector3D_Zero, 0.214, 0.050, 0.250, 0, -5, 5000, 155.3, 0);

  // CITY PRESETS
  ExportPreset('CityStreets', 26, 3, 0.780, -1000, -300, -100, 1.79, 1.12, 0.91, -1100, 0.046, Vector3D_Zero, -1400, 0.028, Vector3D_Zero, 0.250, 0.200, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('CitySubway', 26, 3, 0.740, -1000, -300, -100, 3.01, 1.23, 0.91, -300, 0.046, Vector3D_Zero, 200, 0.028, Vector3D_Zero, 0.125, 0.210, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('CityMuseum', 26, 80.3, 0.820, -1000, -1500, -1500, 3.28, 1.40, 0.57, -1200, 0.039, Vector3D_Zero, -100, 0.034, Vector3D_Zero, 0.130, 0.170, 0.250, 0, -5, 2854.4, 107.5, 0);
  ExportPreset('CityLibrary', 26, 80.3, 0.820, -1000, -1100, -2100, 2.76, 0.89, 0.41, -900, 0.029, Vector3D_Zero, -100, 0.020, Vector3D_Zero, 0.130, 0.170, 0.250, 0, -5, 2854.4, 107.5, 0);
  ExportPreset('CityUnderpass', 26, 3, 0.820, -1000, -700, -100, 3.57, 1.12, 0.91, -800, 0.059, Vector3D_Zero, -100, 0.037, Vector3D_Zero, 0.250, 0.140, 0.250, 0, -7, 5000, 250, 0);
  ExportPreset('CityAbandoned', 26, 3, 0.690, -1000, -200, -100, 3.28, 1.17, 0.91, -700, 0.044, Vector3D_Zero, -1100, 0.024, Vector3D_Zero, 0.250, 0.200, 0.250, 0, -3, 5000, 250, 0);

  // MISC ROOMS
  ExportPreset('Generic', 0, 7.5, 1, -1000, -100, 0, 1.49, 0.83, 1, -2602, 0.007, Vector3D_Zero, 200, 0.011, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Paddedcell', 1, 1.4, 1, -1000, -6000, 0, 0.17, 0.10, 1, -1204, 0.001, Vector3D_Zero, 207, 0.002, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Room', 2, 1.9, 1, -1000, -454, 0, 0.40, 0.83, 1, -1646, 0.002, Vector3D_Zero, 53, 0.003, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Bathroom', 3, 1.4, 1, -1000, -1200, 0, 1.49, 0.54, 1, -370, 0.007, Vector3D_Zero, 1030, 0.011, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Livingroom', 4, 2.5, 1, -1000, -6000, 0, 0.50, 0.10, 1, -1376, 0.003, Vector3D_Zero, -1104, 0.004, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Stoneroom', 5, 11.6, 1, -1000, -300, 0, 2.31, 0.64, 1, -711, 0.012, Vector3D_Zero, 83, 0.017, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Auditorium', 6, 21.6, 1, -1000, -476, 0, 4.32, 0.59, 1, -789, 0.020, Vector3D_Zero, -289, 0.030, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Concerthall', 7, 19.6, 1, -1000, -500, 0, 3.92, 0.70, 1, -1230, 0.020, Vector3D_Zero, -02, 0.029, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Cave', 8, 14.6, 1, -1000, 0, 0, 2.91, 1.30, 1, -602, 0.015, Vector3D_Zero, -302, 0.022, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Arena', 9, 36.2, 1, -1000, -698, 0, 7.24, 0.33, 1, -1166, 0.020, Vector3D_Zero, 16, 0.030, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Hangar', 10, 50.3, 1, -1000, -1000, 0, 10.05, 0.23, 1, -602, 0.020, Vector3D_Zero, 198, 0.030, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Carpettedhallway', 11, 1.9, 1, -1000, -4000, 0, 0.30, 0.10, 1, -1831, 0.002, Vector3D_Zero, -1630, 0.030, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Hallway', 12, 1.8, 1, -1000, -300, 0, 1.49, 0.59, 1, -1219, 0.007, Vector3D_Zero, 441, 0.011, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Stonecorridor', 13, 13.5, 1, -1000, -237, 0, 2.70, 0.79, 1, -1214, 0.013, Vector3D_Zero, 395, 0.020, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Alley', 14, 7.5, 0.300, -1000, -270, 0, 1.49, 0.86, 1, -1204, 0.007, Vector3D_Zero, -4, 0.011, Vector3D_Zero, 0.125, 0.950, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Forest', 15, 38, 0.300, -1000, -3300, 0, 1.49, 0.54, 1, -2560, 0.162, Vector3D_Zero, -229, 0.088, Vector3D_Zero, 0.125, 1, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('City', 16, 7.5, 0.500, -1000, -800, 0, 1.49, 0.67, 1, -2273, 0.007, Vector3D_Zero, -1691, 0.011, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Mountains', 17, 100, 0.270, -1000, -2500, 0, 1.49, 0.21, 1, -2780, 0.300, Vector3D_Zero, -1434, 0.100, Vector3D_Zero, 0.250, 1, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Quarry', 18, 17.5, 1, -1000, -1000, 0, 1.49, 0.83, 1, -10000, 0.061, Vector3D_Zero, 500, 0.025, Vector3D_Zero, 0.125, 0.700, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Plain', 19, 42.5, 0.210, -1000, -2000, 0, 1.49, 0.50, 1, -2466, 0.179, Vector3D_Zero, -1926, 0.100, Vector3D_Zero, 0.250, 1, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Parkinglot', 20, 8.3, 1, -1000, 0, 0, 1.65, 1.50, 1, -1363, 0.008, Vector3D_Zero, -1153, 0.012, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Sewerpipe', 21, 1.7, 0.800, -1000, -1000, 0, 2.81, 0.14, 1, 429, 0.014, Vector3D_Zero, 1023, 0.021, Vector3D_Zero, 0.250, 0, 0.250, 0, -5, 5000, 250, 0);
  ExportPreset('Underwater', 22, 1.8, 1, -1000, -4000, 0, 1.49, 0.10, 1, -449, 0.007, Vector3D_Zero, 1700, 0.011, Vector3D_Zero, 0.250, 0, 1.180, 0.348, -5, 5000, 250, 0);
  ExportPreset('Drugged', 23, 1.9, 0.500, -1000, 0, 0, 8.39, 1.39, 1, -115, 0.002, Vector3D_Zero, 985, 0.030, Vector3D_Zero, 0.250, 0, 0.250, 1, -5, 5000, 250, 0);
  ExportPreset('Dizzy', 24, 1.8, 0.600, -1000, -400, 0, 17.23, 0.56, 1, -1713, 0.020, Vector3D_Zero, -613, 0.030, Vector3D_Zero, 0.250, 1, 0.810, 0.310, -5, 5000, 250, 0);
  ExportPreset('Psychotic', 25, 1, 0.500, -1000, -151, 0, 7.56, 0.91, 1, -626, 0.020, Vector3D_Zero, 774, 0.030, Vector3D_Zero, 0.250, 0, 4, 1, -5, 5000, 250, 0);
  ExportPreset('Dustyroom', 26, 1.8, 0.560, -1000, -200, -300, 1.79, 0.38, 0.21, -600, 0.002, Vector3D_Zero, 200, 0.006, Vector3D_Zero, 0.202, 0.050, 0.250, 0, -10, 13046, 163.3, 0);
  ExportPreset('Chapel', 26, 19.6, 0.840, -1000, -500, 0, 4.62, 0.64, 1.23, -700, 0.032, Vector3D_Zero, -200, 0.049, Vector3D_Zero, 0.250, 0, 0.250, 0.110, -5, 5000, 250, 0);
  ExportPreset('Smallwaterroom', 26, 36.2, 0.700, -1000, -698, 0, 1.51, 1.25, 1.14, -100, 0.020, Vector3D_Zero, 300, 0.030, Vector3D_Zero, 0.179, 0.150, 0.895, 0.190, -7, 5000, 250, 0);
end.
