getMinExtent = function(x, y)
{
  ex = extent(x)
  ey = extent(y)
  
  xmin = max(ex@xmin, ey@xmin)
  xmax = min(ex@xmax, ey@xmax)
  ymin = max(ex@ymin, ey@ymin)
  ymax = min(ex@ymax, ey@ymax)
  
  return(extent(xmin, xmax, ymin, ymax))
}