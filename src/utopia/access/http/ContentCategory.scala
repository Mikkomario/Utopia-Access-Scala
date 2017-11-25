package utopia.access.http

object ContentCategory
{
    // ATTRIBUTES    ----------------
    
    case object Application extends ContentCategory("application")
    case object Audio extends ContentCategory("audio")
    case object Image extends ContentCategory("image")
    case object Message extends ContentCategory("message")
    case object MultiPart extends ContentCategory("multipart")
    case object Text extends ContentCategory("text")
    case object Video extends ContentCategory("video")
    
    case class Custom(override val name: String) extends ContentCategory(name, true)
    
    lazy val existingOptions: Vector[ContentCategory] = Vector(Application, Audio, Image, Message, MultiPart, Text, Video)
    
    
    // OTHER METHODS    -------------
    
    /**
     * Parses a content category string into a content category
     */
    def parse(categoryString: String) = 
    {
        if (categoryString.startsWith("X-"))
        {
            Custom(categoryString.substring(2))
        }
        else
        {
            existingOptions.find { _.name.equalsIgnoreCase(categoryString) }.getOrElse(Custom(categoryString))
        }
    }
}

/**
 * Content categories provide some general information about the specific content type. There are 
 * certain predefined categories along with custom ones
 */
sealed abstract class ContentCategory(val name: String, val isCustom: Boolean = false)
{
    // IMPLEMENTED METHODS    ----------------------
    
    override def toString = if (isCustom) s"X-$name" else name
    
    
    // OPERATORS    --------------------------------
    
    /**
     * Specifies a content type from this content category
     */
    def /(subType: String) = ContentType(this, subType)
}