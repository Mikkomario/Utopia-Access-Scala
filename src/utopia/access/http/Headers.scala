package utopia.access.http

import scala.collection.immutable.Map
import utopia.flow.generic.ModelConvertible
import utopia.flow.datastructure.immutable.Model
import utopia.flow.generic.ValueConversions._
import utopia.flow.generic.FromModelFactory
import utopia.flow.datastructure.template.Property
import utopia.flow.datastructure.template
import java.time.format.DateTimeFormatter
import scala.util.Try
import java.time.Instant
import java.time.ZonedDateTime
import java.time.ZoneOffset
import scala.collection.immutable.HashMap
import utopia.flow.util.Equatable
import java.nio.charset.Charset

object Headers extends FromModelFactory[Headers]
{   
    // ATTRIBUTES    ---------------------
    
    /**
     * An empty set of headers
     */
    val empty = Headers()
    
    
    // COMPUTED PROPERTIES    ------------
    
    /**
     * A set of headers containing a date specification
     */
    def currentDateHeaders = empty.withCurrentDate
    
    
    // OPERATORS    ----------------------
    
    override def apply(model: template.Model[Property]) = 
    {
        // TODO: Handle cases where values are not strings
        val fields = model.attributesWithValue.map { property => (property.name, 
                property.value.stringOr()) }.toMap
        Some(new Headers(fields))
    }
    
    def apply(rawFields: Map[String, String] = HashMap()) = new Headers(rawFields)
}

/**
 * Headers represent headers used in html responses and requests
 * @author Mikko Hilpinen
 * @since 22.8.2017
 */
class Headers(rawFields: Map[String, String] = HashMap()) extends ModelConvertible with Equatable
{
    // ATTRIBUTES    --------------
    
    val fields = rawFields.map { case (key, value) => key.toLowerCase() -> value }
    
    
    // IMPLEMENTED METHODS / PROPERTIES    ---
    
    override def properties = Vector(fields)
    
    override def toModel = Model(fields.toVector.map { case (key, value) => key -> value.toValue });
    
    
    // COMPUTED PROPERTIES    -----
    
    /**
     * The methods allowed for the server resource
     */
    def allowedMethods = commaSeparatedValues("Allow").flatMap { Method.parse }
    
    /**
     * The content types accepted by the client
     */
    def acceptedTypes = commaSeparatedValues("Accept").flatMap { ContentType.parse }
    
    /**
     * The type of the associated content. None if not defined.
     */
    def contentType = semicolonSeparatedValues("Content-Type").headOption.flatMap { ContentType.parse }
    
    /**
     * The character set used in the associated content. None if not defined or unrecognised
     */
    def charset = 
    {
        val cTypeHeader = semicolonSeparatedValues("Content-Type")
        if (cTypeHeader.size > 1)
        {
            Try(Charset.forName(cTypeHeader(1))).toOption
        }
        else
        {
            None
        }
    }
    
    /**
     * The Date general-header field represents the date and time at which the message was 
     * originated, having the same semantics as orig-date in RFC 822. The field value is an 
     * HTTP-date, as described in section 3.3.1; it MUST be sent in RFC 1123 [8]-date format.
     */
    def date = timeHeader("Date")
    
    /**
     * The location of the generated or searched resource. (Usually) contains the whole url.
     */
    def location = apply("Location")
    
    /**
     * The time when the resource was last modified
     */
    def lastModified = timeHeader("Last-Modified")
    
    /**
     * Creates a new set of headers with the updated message date / time
     */
    def withCurrentDate = withDate(Instant.now())
    
    
    // OPERATORS    ---------------
    
    /**
     * Finds the value associated with the specified header name. The value may contain multiple 
     * parts, depending from the header format. Returns None if the header has no value.
     */
    def apply(headerName: String) = fields.get(headerName.toLowerCase())
    
    /**
     * Adds new values to a header. Will not overwrite any existing values.
     */
    def +(headerName: String, values: Seq[String], regex: String): Headers = 
    {
        if (!values.isEmpty)
        {
            this + (headerName, values.reduce { _ + regex + _ })
        }
        else
        {
            this
        }
    }
    
    /**
     * Adds a new value to a header. Will not overwrite any existing values.
     */
    def +(headerName: String, value: String, regex: String = ",") = 
    {
        if (fields.contains(headerName.toLowerCase()))
        {
            // Appends to existing value
            val newValue = apply(headerName).get + regex + value
            Headers(fields + (headerName -> newValue))
        }
        else
        {
            withHeader(headerName, value)
        }
    }
    
    
    
    // OTHER METHODS    -----------
    
    /**
     * Returns multiple values where the original value is split into multiple parts. Returns an 
     * empty vector if there were no values for the header name
     */
    def splitValues(headerName: String, regex: String) = apply(headerName).toVector.flatMap { _.split(regex) }
    
    /**
     * Returns multiple values where the original value is separated with a comma (,). Returns an 
     * empty vector if there were no values for the header name
     */
    def commaSeparatedValues(headerName: String) = splitValues(headerName, ",")
    
    /**
     * Returns multiple values where the original value is separated with a semicolon (;). Returns an 
     * empty vector if there were no values for the header name
     */
    def semicolonSeparatedValues(headerName: String) = splitValues(headerName, ";")
    
    /**
     * Returns a copy of these headers with a new header. Overwrites any previous values on the 
     * targeted header.
     */
    def withHeader(headerName: String, values: Seq[String], regex: String = ","): Headers = 
            withHeader(headerName, values.reduce { _ + regex + _ })
    
    /**
     * Returns a copy of these headers with a new header. Overwrites any previous values on the 
     * targeted header.
     */
    def withHeader(headerName: String, value: String) = new Headers(fields + (headerName -> value))
    
    /**
     * Parses a header field into a time instant
     */
    def timeHeader(headerName: String) = apply(headerName).flatMap { dateStr => 
            Try(Instant.from(DateTimeFormatter.RFC_1123_DATE_TIME.parse(dateStr))).toOption }
    
    /**
     * Parses an instant into correct format and adds it as a header value. Overwrites a previous 
     * version of that header, if there is one.
     */
    def withTimeHeader(headerName: String, value: Instant) = withHeader(headerName, 
            DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.ofInstant(value, ZoneOffset.UTC)));
    
    /**
     * Checks whether a method is allowed for the server side resource
     */
    def allows(method: Method) = allowedMethods.contains(method)
    
    /**
     * Overwrites the list of methods allowed to be used on the targeted resource
     */
    def withAllowedMethods(methods: Seq[Method]) = withHeader("Allow", methods.map { _.toString })
    
    /**
     * Adds a new method to the methods allowed for the targeted resource
     */
    def withMethodAllowed(method: Method) = this + ("Allow", method.toString)
    
    /**
     * Checks whether the client accepts the provided content type
     */
    def accepts(contentType: ContentType) = acceptedTypes.contains(contentType)
    
    /**
     * Overwrites the set of accepted content types
     */
    def withAcceptedTypes(types: Seq[ContentType]) = withHeader("Accept", types.map { _.toString })
    
    /**
     * Adds a new content type to the list of content types accepted by the client
     */
    def withTypeAccepted(contentType: ContentType) = this + ("Accept", contentType.toString)
    
    /**
     * Creates a new headers with the content type (and character set) specified
     * @param contentType the type of the content
     * @param charset then encoding that was used used when the content was written to the response
     */
    def withContentType(contentType: ContentType, charset: Option[Charset] = None) = this + 
            ("Content-Type", contentType.toString + charset.map { ";" + _.name() }.getOrElse(""));
    
    /**
     * Creates a new header with the time when the message associated with this header was originated. 
     * If the message was just created, you may wish to use #withCurrentDate
     */
    def withDate(time: Instant) = withTimeHeader("Date", time);
    
    /**
     * Creates a new header with the time when the resource was last modified
     */
    def withLastModified(time: Instant) = withTimeHeader("Last-Modified", time)
    
    /**
     * Creates a new header with a specified location information
     */
    def withLocation(location: String) = withHeader("Location", location)
    
    // TODO: Implement support for following predefined headers:
    // https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
    /*
     * - Accept-Charset
     * - Accept-Language (?)
     * - Content-Length (?)
     * - Content-Encoding
     * - Content-Language
     * - Expires (?)
     * - Location
     * - If-Modified-Since
     */
}