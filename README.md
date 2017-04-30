# Math
[![Build Status](https://travis-ci.org/Caellian/Math.svg?branch=master)](https://travis-ci.org/Caellian/Math)
[![Download](https://api.bintray.com/packages/caellian/caellian/Math/images/download.svg)](https://bintray.com/caellian/caellian/Math/_latestVersion) <br>
Math library containing classes for matrix and vector calculation.

## Why did I create this library?
I created this library because I couldn't find a single one I liked written in Java or Scala.
I believe there are currently no libraries out there for Java or languages extending it which support Vectors and Matrices of any dimension and are as simple as this one at the same time.

## License & Documentation
You can find license [here](https://github.com/Caellian/Math/blob/master/LICENSE.md) and documentation [here](http://caellian.github.io/Math/).

## Including "Math" in your projects
### Gradle
Add jCenter to repositories closure in build.gradle:
```Groovy
jcenter()
```
After adding following statement to dependencies closure you are good to go:
```Groovy
compile 'hr.caellian:math:1.3.+'
```

### Maven
Maven settings:
```xml
<settings xsi:schemaLocation='http://maven.apache.org/SETTINGS/1.0.0 http://maven.apache.org/xsd/settings-1.0.0.xsd'
xmlns='http://maven.apache.org/SETTINGS/1.0.0' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>
  <profiles>
  	<profile>
  		<repositories>
  			<repository>
  				<snapshots>
  					<enabled>false</enabled>
  				</snapshots>
  				<id>bintray-caellian-caellian</id>
  				<name>bintray</name>
  				<url>http://dl.bintray.com/caellian/caellian</url>
  			</repository>
  		</repositories>
  		<pluginRepositories>
  			<pluginRepository>
  				<snapshots>
  					<enabled>false</enabled>
  				</snapshots>
  				<id>bintray-caellian-caellian</id>
  				<name>bintray-plugins</name>
  				<url>http://dl.bintray.com/caellian/caellian</url>
  			</pluginRepository>
  		</pluginRepositories>
  		<id>bintray</id>
  	</profile>
  </profiles>
  <activeProfiles>
  	<activeProfile>bintray</activeProfile>
  </activeProfiles>
</settings>
```
Add this dependency:
```xml
<dependency>
	<groupId>hr.caellian</groupId>
	<artifactId>math</artifactId>
	<version>[1.3.0,1.4.0)</version>
	<type>pom</type>
</dependency>
```

## TODO List
- [X] Add Vectors and Matrices of other types.
- [ ] Maybe add other content.

## Contributing
Anyone if free to contibute! You can report bugs or add suggestions [here](https://github.com/Caellian/Math/issues).

## Contact
If you have to, you can contact me by sending me an e-mail to tin.svagelj.email@gmail.com.
