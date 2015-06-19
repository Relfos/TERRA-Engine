Program svg_render;

Uses TERRA_Stream, TERRA_FileStream, TERRA_Image, TERRA_SVG, TERRA_PNG;

Var
  Drawing:SVG;
  Img:Image;
  Src:Stream;
Begin
  Drawing := SVG.Create();

  Src := FileStream.Open('minimonlogo.svg');
  Drawing.Load(Src);
  Src.Destroy();

  Img := Drawing.Render(500, 500);
  Drawing.Destroy();

  Img.Save('export.png');
  Img.Destroy();
End.