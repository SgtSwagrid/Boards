package boards.components

import com.raquo.laminar.api.L.*
import com.raquo.laminar.tags.HtmlTag

object Footer:
  
  private val aside = HtmlTag("aside")
  private val nav = HtmlTag("nav")
  
  val footerHeight = 80
  
  def apply() = footerTag (
    className("footer px-10 py-4 border-t bg-base-200 text-base-content border-base-300"),
    position("fixed"),
    bottom("0px"),
    width("100%"),
    height(s"${footerHeight}px"),
    aside (
      className("items-center grid-flow-col"),
      img(width("50px"), height("50px"), src("/assets/images/ui/icon.svg")),
      p (
        "Created by Alec Dorrington",
        br(),
        "Copyright © 2024"
      )
    ),
    nav (
      className("grid-flow-col gap-4 md:place-self-center md:justify-self-end"),
      a (
        img (
          className("hover:brightness-125"),
          width("25px"),
          height("25px"),
          src("/assets/images/ui/footer/github.svg")
        ),
        href("https://github.com/SgtSwagrid/Boards")
      ),
      a (
        img (
          className("hover:brightness-125"),
          width("25px"),
          height("25px"),
          src("/assets/images/ui/footer/discord.svg")
        ),
        href("https://discord.com/invite/5UVjMYw43F")
      )
    )
  )