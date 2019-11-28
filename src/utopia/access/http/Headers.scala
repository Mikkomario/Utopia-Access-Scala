package utopia.access.http

import scala.collection.immutable.Map
import utopia.flow.generic.ModelConvertible
import utopia.flow.datastructure.immutable.Model
import utopia.flow.generic.ValueConversions._
import utopia.flow.generic.FromModelFactory
import utopia.flow.datastructure.template.Property
import utopia.flow.datastructure.template
import java.time.format.DateTimeFormatter

import scala.util.{Success, Try}
import java.time.Instant
import java.time.ZonedDateTime
import java.time.ZoneOffset

import scala.collection.immutable.HashMap
import utopia.flow.util.Equatable
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import java.util.Base64

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
        val fields = model.attributesWithValue.flatMap { property => property.value.string.map { (property.name, _) } }.toMap
        Success(new Headers(fields))
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
    
    override def toModel = Model(fields.toVector.map { case (key, value) => key -> value.toValue })
    
    override def toString = toModel.toString
    
    
    // COMPUTED PROPERTIES    -----
    
    /**
     * @return Whether these headers are empty
     */
    def isEmpty = rawFields.isEmpty
    
    /**
     * @return Whether these headers are not empty
     */
    def nonEmpty = !isEmpty
    
    /**
     * The methods allowed for the server resource
     */
    def allowedMethods = commaSeparatedValues("Allow").flatMap { Method.parse }
    
    /**
     * The content types accepted by the client
     */
    def acceptedTypes = commaSeparatedValues("Accept").flatMap { ContentType.parse }
    
    /**
     * The charsets accepted by the client. Each charset is matched with a weight modifier. Higher 
     * weight charsets should be preferred by the server.
     */
    def acceptedCharsets = commaSeparatedValues("Accept-Charset").map(_.split(";")).flatMap
    {
        set => 
            val weight = (if (set.length > 1) set(1).double else None) getOrElse 1.0
            Try(Charset.forName(set.head)).toOption.map(_ -> weight)
    }.toMap
    
    /**
     * The charset preferred by the client
     */
    def preferredCharset = 
    {
        val accepted = acceptedCharsets
        if (accepted.isEmpty)
            None
        else
            Some(accepted.maxBy(_._2)._1)
    }
    
    /**
     * The charset preferred by the client. UTF 8 if the client doesn't specify any other charset
     */
    def preferredCharsetOrUTF8 = preferredCharset getOrElse StandardCharsets.UTF_8
    
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
     * 	The length of the response body in octets (8-bit bytes)
     */
    def contentLength = apply("Content-Length").flatMap(_.int).getOrElse(0)
    
    /**
      * @return Whether content length information has been provided
      */
    def isContentLengthProvided = isDefined("Content-Length")
    
    /**
     * The Date general-header field represents the date and time at which the message was 
     * originated, having the same semantics as orig-date in RFC 822. The field value is an 
     * HTTP-date, as described in section 3.3.1; it MUST be sent in RFC 1123 [8]-date format.
     */
    def date = timeHeader("Date")
    
    /**
     * @return Whether the date header is defined
     */
    def hasDate = isDefined("Date")
    
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
    
    /**
     * Whether the data is chunked and the content length omitted
     */
    def isChunked = apply("Transfer-Encoding").contains("chunked")
    
    /**
      * @return The provided authorization. Eg. "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==". None if no auth header is provided.
      */
    def authorization = apply("Authorization")
    
    
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
        if (values.nonEmpty)
            this + (headerName, values.reduce { _ + regex + _ })
        else
            this
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
            withHeader(headerName, value)
    }
    
    /**
     * Combines two headers with each other. If the headers have same keys, uses the keys from the 
     * rightmost headers
     */
    def ++(headers: Headers) = Headers(fields ++ headers.fields)
    
    
    // OTHER METHODS    -----------
    
    /**
      * Checks whether speficied header field has been defined
      * @param headerName Header name (case-insensitive)
      * @return Whether such a header has been provided
      */
    def isDefined(headerName: String) = fields.contains(headerName.toLowerCase)
    
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
            DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.ofInstant(value, ZoneOffset.UTC)))
    
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
     * Finds the first accepted type from the provided options
     */
    def getAcceptedType(options: Seq[ContentType]) = 
    {
        val accepted = acceptedTypes
        options.find(accepted.contains)
    }
    
    /**
     * Finds the most preferred accepted charset from the provided options
     */
    def getAcceptedCharset(options: Seq[Charset]) = 
    {
        val accepted = acceptedCharsets.filterKeys(options.contains)
        if (accepted.isEmpty)
            None
        else
            Some(accepted.maxBy(_._2)._1)
    }
    
    /**
     * Overwrites the set of accepted content types
     */
    def withAcceptedTypes(types: Seq[ContentType]) = withHeader("Accept", types.map { _.toString })
    
    /**
     * Adds a new content type to the list of content types accepted by the client
     */
    def withTypeAccepted(contentType: ContentType) = this + ("Accept", contentType.toString)
    
    /**
     * Overwrites the set of accepted charsets
     */
    def withAcceptedCharsets(charsets: Map[Charset, Double]) = withHeader("Accept-Charset", 
            charsets.map{ case (c, w) => s"${c.name()};q=$w" }.toSeq)
    
    /**
     * Overwrites the set of accepted charsets
     */
    def withAcceptedCharsets(charsets: Seq[Charset]) = withHeader("Accept-Charset", charsets.map(_.name()))
    
    /**
     * Adds a new charset to the list of accepted charsets
     */
    def withCharsetAccepted(charset: Charset, weight: Double = 1) = this + ("Accept-Charset", 
            s"${charset.name()};q=$weight")
    
    /**
     * Creates a new headers with the content type (and character set) specified
     * @param contentType the type of the content
     * @param charset then encoding that was used used when the content was written to the response
     */
    def withContentType(contentType: ContentType, charset: Option[Charset] = None) = this + 
            ("Content-Type", contentType.toString + charset.map { ";" + _.name() }.getOrElse(""))
    
    /**
     * Creates a new header with the time when the message associated with this header was originated. 
     * If the message was just created, you may wish to use #withCurrentDate
     */
    def withDate(time: Instant) = withTimeHeader("Date", time)
    
    /**
     * Creates a new header with the time when the resource was last modified
     */
    def withLastModified(time: Instant) = withTimeHeader("Last-Modified", time)
    
    /**
     * Creates a new header with a specified location information
     */
    def withLocation(location: String) = withHeader("Location", location)
    
    /**
      * Creates a copy of these headers that contains authorization
      * @param authString The authorization string. Eg. "Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ=="
      * @return A copy of these headers with authorization
      */
    def withAuthorization(authString: String) = withHeader("Authorization", authString)
    
    /**
      * Creates a copy of these headers that uses basic authentication
      * @param userName Username
      * @param password Password
      * @return A copy of these headers with basic authentication header included
      */
    def withBasicAuthorization(userName: String, password: String) =
    {
        // Encodes the username + password with base64
        val encoded = Base64.getEncoder.encodeToString((userName + ":" + password).getBytes())
        withAuthorization("Basic " + encoded)
    }
    
    // TODO: Implement support for following predefined headers:
    // https://en.wikipedia.org/wiki/List_of_HTTP_header_fields
    /*
     * - Accept-Language (?)
     * - Content-Encoding
     * - Content-Language
     * - Expires (?)
     * - If-Modified-Since
     */
}