package configuration;

import java.io.File;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.logging.Logger;

import org.joda.time.DateTime;
import org.joda.time.DateTimeZone;
import org.joda.time.LocalDateTime;

/**
 * 
 *  This file contains some common utilities function most about Date and directory.
 * 
 * 
 * 
 * 
 */
public class Util 
{
	
	/** 
	 * Saves Running Log  
	 */
	private final static Logger LOG = Logger.getLogger(Logger.GLOBAL_LOGGER_NAME);


	/*
	 *  =================================================================
	 *  
	 * 
	 * ----- Date Use Function
	 * 
	 *  =================================================================
	 */
	/**
	 * Convert UTC To Local
	 * @param pDateTime
	 * @return
	 */
	public static LocalDateTime getLocalDateTime( DateTime pDateTime )
	{    
		//2018-07-11T14:00:00.000Z
        return( new LocalDateTime(DateTimeZone.getDefault().convertUTCToLocal(pDateTime.getMillis())) );
	}
	
	//
	//---------------- Google agenda API use com.google.api.client.util.DateTime different with org.joda.time.DateTime
	//                 
	//
	/**
	 * This function is special created for Google calendar API.
	 * <p>Google calendar API use com.google.api.client.util.DateTime different with org.joda.time.DateTime<br>
	 * <code>pTime<code> come from 
	 * {@link connectors.GoogleConnector#getMeetings(rooms.Room)} which has already convert to UTC local.
	 * @param pTime
	 * @return 
	 */
	public static LocalDateTime getLocalDateTimeFromString(String pTime) 
	{
		//2018-07-11T19:00:00.000+02:00
		String pTimefinal = pTime.substring(0, 23)+"Z";
		
		DateFormat df = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'");
	    Date result = new Date(System.currentTimeMillis());
		try 
		{
			result = df.parse(pTimefinal);
		} 
		catch (ParseException e)
		{
			e.printStackTrace();
		}
		
	    DateTime dateTime = new DateTime(result);
	   
	    return(  dateTime.toLocalDateTime() );
	}
	
	public static String simpleLocalTime( DateTime pDateTime )
	{
		LocalDateTime localDT;
        
        localDT =  new LocalDateTime(DateTimeZone.getDefault().convertUTCToLocal(pDateTime.getMillis()));
        
        return( localDT.toString("HH:mm") );
	}
	
	public static String simpleLocalTime( LocalDateTime pDateTime )
	{
		return( pDateTime.toString("HH:mm"));
	}
	
	
	public static String plainDate( DateTime pDateTime )
	{
		LocalDateTime localDT;
        
        localDT =  new LocalDateTime(DateTimeZone.getDefault().convertUTCToLocal(pDateTime.getMillis()));
        
        return( localDT.toString("dd MMMM YYYY") );
	}
	
	public static String plainDate(LocalDateTime pDate) 
	{
		return( pDate.toString("dd MMMM YYYY") );
	}

	
	public static String shortDate( LocalDateTime pLocalDT )
	{
        return( pLocalDT.toString("dd/MM/YY") );
	}

	/**
	 * 
	 * -- TODO
	 * 
	 * Checks if the given directory exists. If not, create.
	 * 
	 * @param pPath
	 */
	// -- TODO
	public static boolean createDirectoryStructure( String pPath )
	{
		boolean structureExists = false;
		
		Path path = FileSystems.getDefault().getPath( pPath );
		
		//
		// ---- If already exists, do nothing
		// 
		if( Files.exists( path ) )
		{
			return( true );
		}

		//
		// ---- Otherwise create the directory and part structure
		//      if necessary
		//
		try
		{
			Files.createDirectories( path );
			LOG.fine("Creating directory: " + pPath );		
			structureExists = true;
		}
		catch( Exception pException )
		{
			LOG.warning( "Could not create directory: " + pPath );
		}
		
		return( structureExists );
	}

}
