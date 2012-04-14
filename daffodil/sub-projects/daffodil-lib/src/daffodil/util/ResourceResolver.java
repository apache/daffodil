package daffodil.util;

//
// Copyright (C) 2011, 2012 by Michael J. Beckerle, All rights Reserved.
// Permission is granted to use this software for any purpose so long as 
// this copyright is preserved in both the source and binary forms, and
// in any documentation provided with the software. 
// 

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;

import org.w3c.dom.ls.LSInput;
import org.w3c.dom.ls.LSResourceResolver;

public class ResourceResolver implements LSResourceResolver {

  public String filePart(String uri) {
	  String res = null;
	  String parts[] = uri.split("/");
	  if (parts.length == 0) {
		  res = uri;
	  }
	  else {
		  res = parts[parts.length - 1];
	  }
	  return res;
  }

	public LSInput resolveResourceFromClassPath(String type,
			String namespaceURI, String publicId, String systemId,
			String baseURI) {
		Boolean isFound = false;
		InputStream inStream = null;
		String[] prefixesToTry = { "src/xsd/", "srcTest/xsd/", "" };
		for (String prefix : prefixesToTry) {
			String sysIdFileSuffix = filePart(systemId);
			String fn = prefix + sysIdFileSuffix;
//			System.out.print("trying to find NS " + namespaceURI
//					+ " in resource " + fn);
			inStream = this.getClass().getResourceAsStream("/" + fn);
			if (inStream != null) {
				isFound = true;
//				System.out.println("...found!");
				break;
			} else {
//				System.out.println("...nope!");
				continue;
			}
		}
		if (!isFound) return null;
		return new LSInputImpl(publicId, systemId, inStream);
	}
  
  @Override
  public LSInput resolveResource(String type, String namespaceURI, String publicId, String systemId, String baseURI) {
	  LSInput resFromCP = resolveResourceFromClassPath(type, namespaceURI, publicId, systemId, baseURI);
	  if (resFromCP != null) return resFromCP;
	  LSInput resFromFiles = resolveResourceFromFiles(type, namespaceURI, publicId, systemId, baseURI);
	  return resFromFiles;
  }
  

  public LSInput resolveResourceFromFiles(String type, String namespaceURI, String publicId, String systemId, String baseURI) {

    File f = null;
    Boolean isFound = false;
    String[] prefixesToTry = {"../daffodil-lib/src/xsd/", "../daffodil-lib/srcTest/xsd/", ""};
    for (String prefix : prefixesToTry ) {
      String sysIdFileSuffix = filePart(systemId);
      String augmentedSystemId = prefix + sysIdFileSuffix;
      f = new File(augmentedSystemId);
//      String abs = f.getAbsolutePath();
//      System.out.print("trying to find NS "+ namespaceURI + " in file " + abs);
      if (f.exists()) {
        isFound=true;
//        System.out.println("...found!");
        break;
      } else {
//        System.out.println("...nope!");
        continue;
      }
    }
    if (!isFound) {
      System.out.print("Failed to find NS "+ namespaceURI + " in file " + systemId + " in any of the search locations (");
      String comma = "";
      for (String prefix : prefixesToTry ) {
        System.out.print(comma + " " + prefix + "'");
        comma = ",";
      }
      System.out.println(")");
      return null;
    }
    InputStream resourceAsStream;
    try {
      resourceAsStream = new FileInputStream(f); // this.getClass().getResourceAsStream(systemId);
    }
    catch (FileNotFoundException e) {
      throw new RuntimeException(e);
    }
    return new LSInputImpl(publicId, systemId, resourceAsStream);
  }

  protected class LSInputImpl implements LSInput {

    private String publicId;

    private String systemId;

    public String getPublicId() {
      return publicId;
    }

    public void setPublicId(String publicId) {
      this.publicId = publicId;
    }

    @Override
    public String getBaseURI() {
      return null;
    }

    @Override
    public InputStream getByteStream() {
      return null;
    }

    @Override
    public boolean getCertifiedText() {
      return false;
    }

    @Override
    public Reader getCharacterStream() {
      return null;
    }

    @Override
    public String getEncoding() {
      return null;
    }

    @Override
    public String getStringData() {
      synchronized (inputStream) {
        try {
          byte[] input = new byte[inputStream.available()];
          inputStream.read(input);
          String contents = new String(input);
          return contents;
        } catch (IOException e) {
          throw new RuntimeException(e);
        }
      }
    }

    @Override
    public void setBaseURI(String baseURI) {
    }

    @Override
    public void setByteStream(InputStream byteStream) {
    }

    @Override
    public void setCertifiedText(boolean certifiedText) {
    }

    @Override
    public void setCharacterStream(Reader characterStream) {
    }

    @Override
    public void setEncoding(String encoding) {
    }

    @Override
    public void setStringData(String stringData) {
    }

    public String getSystemId() {
      return systemId;
    }

    public void setSystemId(String systemId) {
      this.systemId = systemId;
    }

    public BufferedInputStream getInputStream() {
      return inputStream;
    }

    public void setInputStream(BufferedInputStream inputStream) {
      this.inputStream = inputStream;
    }

    private BufferedInputStream inputStream;

    public LSInputImpl(String publicId, String sysId, InputStream input) {
      this.publicId = publicId;
      systemId = sysId;
      inputStream = new BufferedInputStream(input);
    }

  }

}