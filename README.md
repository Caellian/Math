# Math v3.0.0a
[![Build Status](https://travis-ci.org/Caellian/Math.svg?branch=master)](https://travis-ci.org/Caellian/Math)
[![Codacy Badge](https://api.codacy.com/project/badge/Grade/9a53ff85db9b494eaf22700db0f06394)](https://app.codacy.com/app/Caellian/Math?utm_source=github.com&utm_medium=referral&utm_content=Caellian/Math&utm_campaign=Badge_Grade_Dashboard)
[![Download](https://api.bintray.com/packages/caellian/caellian/Math/images/download.svg)](https://bintray.com/caellian/caellian/Math/_latestVersion)
[![MIT licensed](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE.md)
## Math library containing classes for GLSL calculation and interaction.

### Using Math library
Latest stable version of this library can be found on jCenter.
You can also access alpha (unstable) version using JitPack as described below.

#### Stable
Add jCenter to repositories closure in build.gradle:
```Groovy
repositories {
    jcenter()
}
```
After adding following statement to dependencies closure you are good to go:
```Groovy
dependencies {
    implementation 'hr.caellian:math:2.0.+'
}
```

#### Alpha
Add my private repository to repositories closure in build.gradle:
```Groovy
repositories {
    maven { url "https://dl.bintray.com/caellian/caellian" }
}
```
Then add latest library release tag:
```Groovy
dependencies {
    implementation 'hr.caellian:math:3.0.+'
}
```

### Building
Math uses Gradle as it's build & dependency management system. All you have to do to build the project is download
the repository locally use ```.\gradlew jar``` on Linux or ```./gradlew.bat jar``` on Windows.

Your built files should be then available in 'build\libs' folder.

If you come across any errors during building of files please report them on project [GitHub Issues](https://github.com/Caellian/Math/issues) page.

### Contributing

Source of Math is available on [GitHub](https://github.com/Caellian/Math).

Anyone can contribute to TEMPLATE in accordance with contribution rules stated
in [CONTRIBUTION.md](https://github.com/Caellian/Math/blob/master/CONTRIBUTING.md) file in GitHub repository.

### MIT License
Copyright (C) 2018 Tin Svagelj (a.k.a. Caellian) <tin.svagelj.email@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
