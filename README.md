# 2022MathMechFSharpLessons

Репозиторий домашних заданий на МатМехе (Технологии программирования).

#### Task1 module
1. Реализовать функцию, принимающую два числа (основание и показатель) и вычисляющую степень наивным образом. Реализовать необходимые тесты. `NaivePower`
2. Реализовать функцию, принимающую два числа (основание и показатель) и вычисляющую степень используя быстрое возведение в степень. Реализовать необходимые тесты. `Power`
3. Реализовать функцию, принимающую массив и возвращающую разницу между самым большим и самым маленьким элементами в этом массиве. Добавить тесты. `Dispersion`
4. Реализовать функцию, которая принимает два целых числа и возвращает массив нечётных чисел, лежащих строго между ними. Реализовать тесты. `OddBetween`

#### Task2 module
Для двух типов, реализующих список, которые были придуманы на паре.
1. Реализовать сортировку пузырьком: функция принимает список и возвращает отсортированный список. Добавить необходимые тесты. `FuncList.BubbleSort` и `OOPList.BubbleSort`
2. Реализовать сортировку Хоара (быструю сотрировку): функция принимает список и возвращает отсортированный список. Добавить необходимые тесты. `FuncList.QuickSort` и `OOPList.QuickSort`
3. Реализовать конкатенацию двух списков: функуия принимает два списка и возвращает третий —- конкатенацию двух входных. `FuncList.Concat` и `OOPList.Concat`
Подробнее [здесь](src/2022MathMechFSharpLessons/README.md##Task2).

#### Task3 module
Домашняя работа 3. Про деревья.
1. Описать алгебраический тип, задающий дерево, в котором внутренний узел может иметь произвольное количество сыновей. Листья и внутренние узлы хранят некоторые значения произвольного (одинакового для всех) типа. `Tree`
2. Реализовать функцию, которая по дереву (тип, описанный выше), находит количество различных элементов, хранящихся в узлах. Использовать рекурсию, не использовать мутабельные переменные. `Tree.CountDifferent`
3. Реализовать функцию, которая по дереву (тип, описанный выше), строит список, содержащий все значения из всех узлов. Для сипска использовать тип MyList из предыдущей задачи. Рекурсия — хорошо, мутабельные переменные — плохо. `Tree.GetList`
Подробнее [здесь](src/2022MathMechFSharpLessons/README.md##Task3).

#### Task4 module
Домашка 4. 
1. Используя алгебраические типы данных реализовать деревья, необходимые для представления разреженных матриц и разреженных векторов. Предусмотреть возможность хранить в векторах и матрицах значения различных типов (бывают матрицы со строками, бывают с целыми числами и т.д.) `VectorBinTree<'elementType>` и `MatrixQuadTree<'elementType>`
2. Реализовать тип "вектор" и тип "матрица" с использованием реализованных ранее деревьев. `Vector<'elementType>` и `Matrix<'elementType>`
3. Реализовать функцию умножения вектора на матрицу. 
4. Не забыть все протестировать.

# Как запустить какую-то функцию из перечисленных выше?

1. Собрать проект: `./build.sh DotnetBuild`
2. По желанию, можно протестировать: `./build.sh DotnetTest`
3. Запустить: `./src/2022MathMechFSharpLessons/bin/Debug/net6.0/2022MathMechFSharpLessons`
Этот исполняемый файл выполняет функции из task1, а функции из других заданий - нет

## Builds


GitHub Actions |
:---: |
[![GitHub Actions](https://github.com/Pavlova-Alena-student/2022MathMechFSharpLessons/workflows/Build%20master/badge.svg)](https://github.com/Pavlova-Alena-student/2022MathMechFSharpLessons/actions?query=branch%3Amaster) |
[![Build History](https://buildstats.info/github/chart/Pavlova-Alena-student/2022MathMechFSharpLessons)](https://github.com/Pavlova-Alena-student/2022MathMechFSharpLessons/actions?query=branch%3Amaster) |

## NuGet

Package | Stable | Prerelease
--- | --- | ---
2022MathMechFSharpLessons | [![NuGet Badge](https://buildstats.info/nuget/2022MathMechFSharpLessons)](https://www.nuget.org/packages/2022MathMechFSharpLessons/) | [![NuGet Badge](https://buildstats.info/nuget/2022MathMechFSharpLessons?includePreReleases=true)](https://www.nuget.org/packages/2022MathMechFSharpLessons/)


---

### Developing

Make sure the following **requirements** are installed on your system:

- [dotnet SDK](https://www.microsoft.com/net/download/core) 3.0 or higher
- [Mono](http://www.mono-project.com/) if you're on Linux or macOS.

or

- [VSCode Dev Container](https://code.visualstudio.com/docs/remote/containers)


---

### Environment Variables

- `CONFIGURATION` will set the [configuration](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x#options) of the dotnet commands.  If not set, it will default to Release.
  - `CONFIGURATION=Debug ./build.sh` will result in `-c` additions to commands such as in `dotnet build -c Debug`
- `GITHUB_TOKEN` will be used to upload release notes and NuGet packages to GitHub.
  - Be sure to set this before releasing
- `DISABLE_COVERAGE` Will disable running code coverage metrics.  AltCover can have [severe performance degradation](https://github.com/SteveGilham/altcover/issues/57) so it's worth disabling when looking to do a quicker feedback loop.
  - `DISABLE_COVERAGE=1 ./build.sh`


---

### Building


```sh
> build.cmd <optional buildtarget> // on windows
$ ./build.sh  <optional buildtarget>// on unix
```

---

### Build Targets


- `Clean` - Cleans artifact and temp directories.
- `DotnetRestore` - Runs [dotnet restore](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-restore?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- [`DotnetBuild`](#Building) - Runs [dotnet build](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-build?tabs=netcore2x) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019).
- `DotnetTest` - Runs [dotnet test](https://docs.microsoft.com/en-us/dotnet/core/tools/dotnet-test?tabs=netcore21) on the [solution file](https://docs.microsoft.com/en-us/visualstudio/extensibility/internals/solution-dot-sln-file?view=vs-2019.).
- `GenerateCoverageReport` - Code coverage is run during `DotnetTest` and this generates a report via [ReportGenerator](https://github.com/danielpalme/ReportGenerator).
- `WatchApp` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) on the application. Useful for rapid feedback loops.
- `WatchTests` - Runs [dotnet watch](https://docs.microsoft.com/en-us/aspnet/core/tutorials/dotnet-watch?view=aspnetcore-3.0) with the test projects. Useful for rapid feedback loops.
- `GenerateAssemblyInfo` - Generates [AssemblyInfo](https://docs.microsoft.com/en-us/dotnet/api/microsoft.visualbasic.applicationservices.assemblyinfo?view=netframework-4.8) for libraries.
- `CreatePackages` - Runs the packaging task from [dotnet-packaging](https://github.com/qmfrederik/dotnet-packaging). This creates applications for `win-x64`, `osx-x64` and `linux-x64` - [Runtime Identifiers](https://docs.microsoft.com/en-us/dotnet/core/rid-catalog).  
    - Bundles the `win-x64` application in a .zip file.
    - Bundles the `osx-x64` application in a .tar.gz file.
    - Bundles the `linux-x64` application in a .tar.gz file.
- `GitRelease` - Creates a commit message with the [Release Notes](https://fake.build/apidocs/v5/fake-core-releasenotes.html) and a git tag via the version in the `Release Notes`.
- `GitHubRelease` - Publishes a [GitHub Release](https://help.github.com/en/articles/creating-releases) with the Release Notes and any NuGet packages.
- `FormatCode` - Runs [Fantomas](https://github.com/fsprojects/fantomas) on the solution file.
- [`Release`](#Releasing) - Task that runs all release type tasks such as `GitRelease` and `GitHubRelease`. Make sure to read [Releasing](#Releasing) to setup your environment correctly for releases.

---


### Releasing

- [Start a git repo with a remote](https://help.github.com/articles/adding-an-existing-project-to-github-using-the-command-line/)

```sh
git add .
git commit -m "Scaffold"
git remote add origin https://github.com/user/MyCoolNewApp.git
git push -u origin master
```

- [Create a GitHub OAuth Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/)
  - You can then set the `GITHUB_TOKEN` to upload release notes and artifacts to github
  - Otherwise it will fallback to username/password

- Then update the `CHANGELOG.md` with an "Unreleased" section containing release notes for this version, in [KeepAChangelog](https://keepachangelog.com/en/1.1.0/) format.


NOTE: Its highly recommend to add a link to the Pull Request next to the release note that it affects. The reason for this is when the `RELEASE` target is run, it will add these new notes into the body of git commit. GitHub will notice the links and will update the Pull Request with what commit referenced it saying ["added a commit that referenced this pull request"](https://github.com/TheAngryByrd/MiniScaffold/pull/179#ref-commit-837ad59). Since the build script automates the commit message, it will say "Bump Version to x.y.z". The benefit of this is when users goto a Pull Request, it will be clear when and which version those code changes released. Also when reading the `CHANGELOG`, if someone is curious about how or why those changes were made, they can easily discover the work and discussions.



Here's an example of adding an "Unreleased" section to a `CHANGELOG.md` with a `0.1.0` section already released.

```markdown
## [Unreleased]

### Added
- Does cool stuff!

### Fixed
- Fixes that silly oversight

## [0.1.0] - 2017-03-17
First release

### Added
- This release already has lots of features

[Unreleased]: https://github.com/user/MyCoolNewApp.git/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/user/MyCoolNewApp.git/releases/tag/v0.1.0
```

- You can then use the `Release` target, specifying the version number either in the `RELEASE_VERSION` environment
  variable, or else as a parameter after the target name.  This will:
  - update `CHANGELOG.md`, moving changes from the `Unreleased` section into a new `0.2.0` section
    - if there were any prerelease versions of 0.2.0 in the changelog, it will also collect their changes into the final 0.2.0 entry
  - make a commit bumping the version:  `Bump version to 0.2.0` and adds the new changelog section to the commit's body
  - push a git tag
  - create a GitHub release for that git tag


macOS/Linux Parameter:

```sh
./build.sh Release 0.2.0
```

macOS/Linux Environment Variable:

```sh
RELEASE_VERSION=0.2.0 ./build.sh Release
```
