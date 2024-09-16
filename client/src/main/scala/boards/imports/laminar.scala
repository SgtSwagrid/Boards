package boards.imports

object laminar:
    
  export scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
  
  export org.scalajs.dom.{
    document,
    window,
    html,
    KeyCode,
    //MouseEvent,
    //HTMLImageElement,
    //CanvasRenderingContext2D,
    //DOMRect,
  }
  export org.scalajs.dom.html.Canvas
  
  export com.raquo.laminar.api.L.{*, given}
  export com.raquo.laminar.api.features.unitArrows
  export com.raquo.laminar.nodes.{ReactiveElement, ReactiveHtmlElement, ReactiveSvgElement}
  export com.raquo.laminar.tags.HtmlTag
  export com.raquo.laminar.codecs.StringAsIsCodec
  
  //export io.laminext.websocket.WebSocket
  
  export io.laminext.syntax.core.*
  
  export io.laminext.fetch.circe.{Fetch, jsonRequestBody}
  export io.laminext.websocket.circe.{WebSocket, WebSocketEvent, WebSocketError}
  export io.laminext.fetch.circe.fetchEventStreamBuilderSyntaxCirce
  export io.laminext.websocket.circe.webSocketReceiveBuilderSyntax
  
  export boards.components.{
    ExpandingButton,
    Footer,
    InputField,
    Navbar,
    SVG,
    Tabs,
  }
  export boards.components.Tabs.Tab
  export boards.util.Navigation
  
  export scala.concurrent.ExecutionContext.Implicits.global