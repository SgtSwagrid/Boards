package boards.components

import boards.imports.laminar.{*, given}

object SVG:
  
  def Username = svg.svg (
    svg.xmlns("http://www.w3.org/2000/svg"),
    svg.viewBox("0 0 16 16"),
    svg.fill("currentColor"),
    svg.className("w-4 h-4 opacity-70"),
    svg.path (
      svg.d("M8 8a3 3 0 1 0 0-6 3 3 0 0 0 0 6ZM12.735 14c.618 0 1.093-.561.872-1.139a6.002 6.002 0 0 0-11.215 0c-.22.578.254 1.139.872 1.139h9.47Z"),
    ),
  )
  
  def Email = svg.svg (
    svg.xmlns("http://www.w3.org/2000/svg"),
    svg.viewBox("0 0 16 16"),
    svg.fill("currentColor"),
    svg.className("w-4 h-4 opacity-70"),
    svg.path (
      svg.d("M2.5 3A1.5 1.5 0 0 0 1 4.5v.793c.026.009.051.02.076.032L7.674 8.51c.206.1.446.1.652 0l6.598-3.185A.755.755 0 0 1 15 5.293V4.5A1.5 1.5 0 0 0 13.5 3h-11Z")
    ),
    svg.path (
      svg.d("M15 6.954 8.978 9.86a2.25 2.25 0 0 1-1.956 0L1 6.954V11.5A1.5 1.5 0 0 0 2.5 13h11a1.5 1.5 0 0 0 1.5-1.5V6.954Z")
    ),
  )
  
  def Password = svg.svg (
    svg.xmlns("http://www.w3.org/2000/svg"),
    svg.viewBox("0 0 16 16"),
    svg.fill("currentColor"),
    svg.className("w-4 h-4 opacity-70"),
    svg.path (
      svg.fillRule("evenodd"),
      svg.clipRule("evenodd"),
      svg.d("M14 6a4 4 0 0 1-4.899 3.899l-1.955 1.955a.5.5 0 0 1-.353.146H5v1.5a.5.5 0 0 1-.5.5h-2a.5.5 0 0 1-.5-.5v-2.293a.5.5 0 0 1 .146-.353l3.955-3.955A4 4 0 1 1 14 6Zm-4-2a.75.75 0 0 0 0 1.5.5.5 0 0 1 .5.5.75.75 0 0 0 1.5 0 2 2 0 0 0-2-2Z")
    ),
  )
  
  def Cross = svg.svg (
    svg.xmlns("http://www.w3.org/2000/svg"),
    svg.className("h-6 w-6"),
    svg.fill("none"),
    svg.viewBox("0 0 24 24"),
    svg.stroke("currentColor"),
    svg.path (
      svg.strokeLineCap("round"),
      svg.strokeLineJoin("round"),
      svg.strokeWidth("2"),
      svg.d("M6 18L18 6M6 6l12 12"),
    ),
  )
  
  def Swap = svg.svg (
    svg.xmlns("http://www.w3.org/2000/svg"),
    svg.width("24"),
    svg.height("24"),
    svg.x("0"),
    svg.y("0"),
    svg.viewBox("0 0 24 24"),
    svg.style("enable-background:new 0 0 512 512"),
    svg.xmlSpace("preserve"),
    svg.path (
      svg.fill("currentColor"),
      svg.d("M8 16H4l6 6V2H8zm6-11v17h2V8h4l-6-6z"),
    )
  )