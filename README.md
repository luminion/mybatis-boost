# mybatis-plus-enhancer
enhancer of mybatis-plus
* 代码生成器兼容`mybatis-plus代码生成器`的所有配置项目, 并提供更多额外的可选配置项
* 代码生成器提供`新增DTO`,`修改DTO`,`导入DTO`,`导出DTO`,`出参VO`的额外生成
* 代码生成器支持选择性生成`增删查改导入导出`相关方法及配套类 
* 代码生成器提供`代码注入`的方式生成生成额外方法及逻辑, 生成的代码可在不添加额外依赖的情况下运行
* 提供多种`IService`的增强实现, 提供更多可用方法, 并完全兼容`IService`, 可无缝迁移衔接
* 支持通过`属性`+`特殊后缀`的方式自动映射不同类型的查询
* 支持过前端传参生成`防注入的动态sql`, 并支持条件嵌套

[依赖](#maven依赖)   
[增强型代码生成器](#代码生成器)   
[动态Sql工具](#动态sql工具SqlHelper)  
[IService增强](#dynamicservicet-v)   
[BaseMapper增强](#dynamicmappert-v-s)   


## Maven依赖

### SpringBoot3
```xml
<!-- 依赖版本管理 -->
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>mybatis-plus-enhancer</artifactId>
            <version>1.0.0</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>

<dependencies>
    <!--mybatis-plus-enhancer-->
    <dependency>
        <groupId>io.github.bootystar</groupId>
        <artifactId>mybatis-plus-enhancer</artifactId>
    </dependency>
    <!-- spring boot3 引入 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-spring-boot3-starter</artifactId>
    </dependency>
</dependencies>
```

### SpringBoot2
```xml
<!-- 依赖版本管理 -->
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>mybatis-plus-enhancer</artifactId>
            <version>1.0.0</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>

<dependencies>
    <!--mybatis-plus-enhancer-->
    <dependency>
        <groupId>io.github.bootystar</groupId>
        <artifactId>mybatis-plus-enhancer</artifactId>
    </dependency>
    <!-- spring boot2 引入 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-boot-starter</artifactId>
    </dependency>
</dependencies>
```

### 可选项

#### 分页插件依赖
```xml
<!-- (可选)分页插件 jdk11+引入 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-jsqlparser</artifactId>
</dependency>
<!-- (可选)分页插件 jdk8+引入 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-jsqlparser-4.9</artifactId>
</dependency>
```

#### 代码生成器依赖
```xml
<!-- (可选)代码生成器 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-generator</artifactId>
    <!-- (可选)生成器代码在test目录下时,可使用test作用域 -->
    <!--<scope>test</scope>-->
</dependency>
<!-- (可选)代码生成器-模板引擎 -->
<dependency>
    <groupId>org.apache.velocity</groupId>
    <artifactId>velocity-engine-core</artifactId>
    <!-- (可选)生成器代码在test目录下时,可使用test作用域 -->
    <!--<scope>test</scope>-->
</dependency>
```

#### Excel依赖
```xml
<!-- (可选)Excel导入导出 -->
<dependency>
    <groupId>com.alibaba</groupId>
    <artifactId>easyexcel</artifactId>
</dependency>
```

### `pom.xml`示例
```xml
<!-- 依赖版本管理 -->
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>mybatis-plus-enhancer</artifactId>
            <version>1.0.0</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>

<dependencies>
    <!--mybatis-plus-enhancer-->
    <dependency>
        <groupId>io.github.bootystar</groupId>
        <artifactId>mybatis-plus-enhancer</artifactId>
    </dependency>
    <!-- spring boot3 引入 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-spring-boot3-starter</artifactId>
    </dependency>
    <!-- spring boot2 引入 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-boot-starter</artifactId>
    </dependency>
    <!-- (可选)分页插件 jdk11+引入 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-jsqlparser</artifactId>
    </dependency>
    <!-- (可选)分页插件 jdk8+引入 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-jsqlparser-4.9</artifactId>
    </dependency>
    <!-- (可选)代码生成器 -->
    <dependency>
        <groupId>com.baomidou</groupId>
        <artifactId>mybatis-plus-generator</artifactId>
        <!-- (可选)生成器代码在test目录下时,可使用test作用域 -->
        <!--<scope>test</scope>-->
    </dependency>
    <!-- (可选)代码生成器-模板引擎 -->
    <dependency>
        <groupId>org.apache.velocity</groupId>
        <artifactId>velocity-engine-core</artifactId>
        <!-- (可选)生成器代码在test目录下时,可使用test作用域 -->
        <!--<scope>test</scope>-->
    </dependency>
    <!-- (可选)Excel导入导出 -->
    <dependency>
        <groupId>com.alibaba</groupId>
        <artifactId>easyexcel</artifactId>
    </dependency>
</dependencies>
```

### SNAPSHOT仓库地址
#### 若需引入`SNAPSHOT`快照版本, 需配置快照仓库地址
```xml
<repositories>
    <repository>
        <id>snapshot</id>
        <name>snapshot</name>
        <url>https://s01.oss.sonatype.org/content/repositories/snapshots/</url>
        <snapshots>
            <enabled>true</enabled>
        </snapshots>
    </repository>
</repositories>
```
#### 若使用阿里云仓库, 需在maven的`settings.xml`文件中配置`!snapshots`
```xml
<mirror>
  <id>aliyunmaven</id>
  <mirrorOf>*,!snapshots</mirrorOf>
  <name>aliyun</name>
  <url>https://maven.aliyun.com/repository/public</url>
</mirror>
```
### RELEASE仓库地址
中央仓库同步各镜像仓库有延迟, 若无法拉取, 可通过配置或直接访问该地址下载
```xml
<repositories>
    <repository>
        <id>release</id>
        <name>release</name>
        <url>https://s01.oss.sonatype.org/content/repositories/releases/</url>
    </repository>
</repositories>
```

## 代码生成器

### 代码生成(Lambda链式)
对`Lambda表达式`不熟悉的见[代码生成(编码式)](#代码生成编码式)
```java
import io.github.bootystar.mybatisplus.generate.GeneratorHelper;
```
```java
String url = "jdbc:postgresql://localhost:5432/test?useUnicode=true&characterEncoding=UTF-8";
String username = "postgres";
String password = "root";
GeneratorHelper
//        .extraCodeGenerator(url, username, password) // 额外代码生成器
//        .dynamicSqlGenerator(url, username, password) // 动态SQL生成器
        .dynamicFieldGenerator(url, username, password) // 动态字段生成器
        .initialize() // 初始化常用配置
        .pkg(pkg -> pkg.parent("io.github.bootystar" ))// 父包名
//        .mapperXmlResource("static/mapper") // mapper.xml文件在Resources下的路径
        .execute("sys_user" )// 要生成的表(不输入为全部)
;
```

### 可选配置项
```java
String url = "jdbc:postgresql://localhost:5432/test?useUnicode=true&characterEncoding=UTF-8";
String username = "postgres";
String password = "root";
GeneratorHelper
        // .extraCodeGenerator(url, username, password) // 额外代码生成器
        // .dynamicSqlGenerator(url, username, password) // 动态SQL生成器
        .dynamicFieldGenerator(url, username, password) // 动态字段生成器
        .enableGlobalFileOverwrite() // 全局文件覆盖生成(覆盖所有的文件)
        .mapperXmlResource("static/mapper") // mapper.xml文件在Resources下的路径
        .initialize() // 初始化常用配置
        .custom(custom -> {
        // 自定义配置
            custom
                .enableJakartaApi() // 启用Jakarta API, springboot3及以上需要开启
                .enableFileOverride() // 文件覆盖生成(DTO、VO)
                .disableInsert() // 不生成新增
                .disableUpdate() // 不生成更新
                .disableDelete() // 不生成删除
                .disableSelect() // 不生成查询
                .disableImport() // 不生成导入
                .disableExport() // 不生成导出
                // 略... 更多可配置项见下文说明
        ;})
        .dataSource(dataSource -> {
        // 数据源配置(参考mybatis-plus官方文档)
        })
        .global(global -> {
        // 全局配置(参考mybatis-plus官方文档)
        })
        .pkg(pkg -> {
        // 包配置(参考mybatis-plus官方文档)
        })
        .strategy(strategy -> {
        // 策略配置(参考mybatis-plus官方文档)
        })
        .entity(entity -> {
        // 实体类配置(参考mybatis-plus官方文档)
        })
        .mapper(mapper -> {
        // mapper配置(参考mybatis-plus官方文档)
        })
        .service(service -> {
        // service配置(参考mybatis-plus官方文档)
        })
        .controller(controller -> {
        // controller配置(参考mybatis-plus官方文档)
        })
        .execute("sys_user") // 要生成的表(不输入为全部)
        ;
```

### 自定义配置内容
* 通用配置项:[BaseBuilder.java](src/main/java/io/github/bootystar/mybatisplus/generate/config/builder/BaseBuilder.java)
* 特殊配置项:[BaseEnhanceBuilder.java](src/main/java/io/github/bootystar/mybatisplus/generate/config/builder/BaseEnhanceBuilder.java)
* 额外代码生成器特殊配置项:[ExtraCodeBuilder.java](src/main/java/io/github/bootystar/mybatisplus/generate/config/builder/ExtraCodeBuilder.java)
* 动态sql生成器特殊配置项:[DynamicSqlBuilder.java](src/main/java/io/github/bootystar/mybatisplus/generate/config/builder/DynamicSqlBuilder.java)
* 动态字段生成器特殊配置项:[DynamicFieldBuilder.java](src/main/java/io/github/bootystar/mybatisplus/generate/config/builder/DynamicFieldBuilder.java)

自定义配置示例:(仅供参考, 若有变动不再更新, 详见源码)
```java
custom
    // 文件相关
    .enableFileOverride() // 文件覆盖生成(DTO、VO)
    .disableInsert() // 不生成新增
    .disableUpdate() // 不生成更新
    .disableDelete() // 不生成删除
    .disableSelect() // 不生成查询
    .disableImport() // 不生成导入
    .disableExport() // 不生成导出
    .package4DTO("dto") // DTO的包名
    .path4DTO("C:/Project/test21/") // DTO的路径(全路径或相对路径)
    .package4VO("vo") // VO的包名
    .path4VO("C:/Project/test21/") // VO的路径(全路径或相对路径)
    .editExcludeColumns("create_time", "update_time") // 新增/修改时忽略的字段
    // controller额外生成项
    .baseUrl("/api") // 请求url前缀
    .enableCrossOrigins() // 启用跨域
    .enableJakartaApi() // 启用Jakarta API, springboot3以上需要开启
    .enableAutoWired() // 使用@Autowired替换@Resource
    .returnMethod(R1::new) // 返回值对象封装的方法
    .pageMethod(P1::of) // 分页对象封装的方法(需要接收IPage作为参数)
    .disableRestful() // 禁用restful
    .disableRequestBody() // 禁用请求体
    .disableValidated() // 禁用参数校验
    .disablePostQuery() // 复杂查询不使用post请求(使用get请求, 并关闭@RequestBody)
    // mapper额外生成项
    .sortColumnClear() // 清空排序
    .sortColumn("create_time",true) // 添加排序(字段,是否倒序)
    .sortColumn("id",true) // 添加排序(字段,是否倒序)
    // 特殊配置项, 因不同生成器而异
    .disableOverrideMethods() // 不生成重写的父类方法
    .withMapSelectDTO() // 使用Map作为查询入参DTO
    .withSqlHelperSelectDTO() // 使用SqlHelper作为查询入参DTO
    .extraFieldStrategy(new ExtraFieldStrategyDefault()) // 自定义额外字段策略(字段何时生成/不生成对应后缀的额外字段)
    .extraFieldSuffixBuilder(builder -> { 
    // 该项默认无需配置, 配置后, 只会根据已配置的字段生成额外后缀, 未配置的类型不会生成后缀
                    builder
                        .defaultSuffix() // 添加默认的后缀字符(默认生成LIKE,IN,<=,>=的特殊后缀)
                        .defaultSuffixFull() // 添加所有默认的后缀字符
                        .ne("Ne") // 不等于字段额外后缀
                        .lt("Lt") // 小于字段额外后缀
                        .le("Le") // 小于等于字段额外后缀
                        .ge("Ge") // 大于等于字段额外后缀
                        .gt("Gt") // 大于字段额外后缀
                        .like("Like") // 模糊匹配字段额外后缀
                        .notLike("NotLike") // 反模糊匹配字段额外后缀
                        .in("In") // 包含字段额外后缀
                        .notIn("NotIn") // 不包含字段额外后缀
                        .isNull("IsNull") // 空字段额外后缀
                        .isNotNull("IsNotNull") // 非空字段额外后缀
        ;})// 自定义字段额外后缀
```

### 代码生成(编码式)
```java
import io.github.bootystar.mybatisplus.generate.generator.impl.*;
```
```java
String url = "jdbc:postgresql://localhost:5432/test?useUnicode=true&characterEncoding=UTF-8";
String username = "postgres";
String password = "root";
//ExtraCodeGenerator generator = new ExtraCodeGenerator(url, username, password); // 额外代码生成器
//DynamicSqlGenerator generator = new DynamicSqlGenerator(url, username, password); // 动态SQL生成器
DynamicFieldGenerator generator = new DynamicFieldGenerator(url, username, password); // 动态字段生成器

generator.enableGlobalFileOverwrite() // 全局文件覆盖生成(覆盖所有的文件)
        .mapperXmlResource("static/mapper") // mapper.xml文件在Resources下的路径
        .initialize() // 初始化常用配置
;
generator.getCustomConfigBuilder() // 自定义配置
        .enableJakartaApi() // ...略
;
generator.getDataSourceConfigBuilder() // 数据源配置(参考mybatis-plus官方文档)
    //.driverClassName("org.postgresql.Driver")
;
generator.getGlobalConfigBuilder() // 全局配置(参考mybatis-plus官方文档)
    //.author("bootystar")
;
generator.getPackageConfigBuilder() // 包配置(参考mybatis-plus官方文档)
    //.parent("io.github.bootystar")
;
generator.getStrategyConfigBuilder() // 策略配置(参考mybatis-plus官方文档)
    //.addTablePrefix("sys_")
;
generator.getStrategyConfigBuilder().entityBuilder() // 实体类配置(参考mybatis-plus官方文档)
    //.enableLombok()
;
generator.getStrategyConfigBuilder().mapperBuilder() // mapper配置(参考mybatis-plus官方文档)
    //.enableBaseResultMap()
;
generator.getStrategyConfigBuilder().serviceBuilder() // service配置(参考mybatis-plus官方文档)
    //.formatServiceFileName("%sService")
;
generator.getStrategyConfigBuilder().controllerBuilder() // controller配置(参考mybatis-plus官方文档)
    //.enableRestStyle()
;
generator.execute("sys_user"); // 要生成的表(不输入为全部)
```

### 生成器实现

#### 额外代码生成器`ExtraCodeGenerator`
优点:
* 该生成器增强方式为`代码注入`, 可生成后直接复制代码到其他`mybatis-plus`项目使用,可移植性强
* 运行时除`mybatis-plus`外无其他依赖, 依赖耦合低
* 支持通过`属性`+`特殊后缀`的方式自动映射不同类型的查询

缺点:
* `Service`和`ServiceImpl`内硬编码了`DTO`,`VO`
* `mapper.xml`内代码量较多
* 若生成后的实体数据库模型发生变化, 需要修改对应的`mapper.xml`和`SelectDTO`

#### 动态sql生成器`DynamicSqlGenerator`
优点:
* 默认使用`SqlHelper`入参, 支持`lambda`链式调用, `灵活`性极高
* 可动态映射`属性`和`值`为查询条件, 并支持嵌套子条件
* 可动态映射`排序属性`和`升降`序
* 可添加`非本表字段`的动态映射
* 支持直接使用`实体类`和`Map`入参,可根据入参动态映射
* 可自定义额外参数, 在`mapper.xml`中可直接使用
* `Service`继承实现, 无需实现, 无额外代码
* `ServiceImpl`继承实现, 无需实现, 无额外代码
* `mapper.xml`中内容`简洁`且`兼容性`强, 可无缝衔接自行编写的sql

缺点:
* 需要`mybatis-plus-enhancer`依赖, 部分低版本`mybatis-plus`需要升级后使用
* 前端传参较复杂

#### 动态字段生成器`DynamicFieldGenerator`
优点:
* 入参为`SqlHelper`时, 兼容`DynamicSqlGenerator`的动态映射功能
* 支持通过`属性`+`特殊后缀`的方式自动映射不同类型的查询
* 可自定义额外参数, 在`mapper.xml`中可直接使用
* `Service`继承实现, 无需实现, 无额外代码
* `ServiceImpl`继承实现, 无需实现, 无额外代码
* `mapper.xml`中内容`简洁`且`兼容性`强, 可无缝衔接自行编写的sql

缺点:
* 需要`mybatis-plus-enhancer`依赖, 部分低版本`mybatis-plus`需要升级后使用

## 动态sql工具`SqlHelper`

### 后端方法
该工具用于生成sql片段, 支持Object入参  
该工具条件底层为树状结构, 入参可以进行子条件的多层嵌套  
嵌套子条件时,父条件必须为有效条件(即能映射对应字段的条件)  
该类含以下方法用于生成sql片段
* `<T>of()`静态方法, 通过指定对象的属性/值映射创建SqlHelper, 可指定泛型用于方法匹配入参
* `requiredNext()` 设置下一个条件为必定生效的条件
* `or()`设置下一个条件与最后一个条件的关系为or
* `eq()`等于
* `ne()`不等于
* `gt()`大于
* `ge()`大于等于
* `lt()`小于
* `le()`小于等于
* `like()`模糊匹配
* `notLike()`反模糊匹配
* `isNull()`为空
* `isNotNull()`非空
* `in()`包含
* `notIn()`不包含
* `orderByAsc()`排序升序
* `orderByDesc()`排序降序
* `condition()`添加ISqlCondition(条件的原始封装类, 推荐使用`ConditionG`)
* `sort()`添加ISqlSort(排序的原始封装类, 推荐使用`SortG`)
* `with()`添加并融合另一个SqlHelper所有条件(包含子条件)(`ISqlTree`为`SqlHelper`的父类)
* `withChild()`将另一个SqlHelper所有条件(包含子条件)添加为本对象的子条件
* `wrap()`包装SqlHelper, 添加指定DynamicService服务的查询方法

使用示例
```java

    @Resource
    private ISysUserService baseService;
    
    public void example() {
        
        // 根据指定实体类\map\SqlHelper创建sqlHelper
        HashMap<String, Object> map = new HashMap<>();
        map.put("name","张三"); // 姓名= 张三
        map.put("ageIn", Arrays.asList(1,2,3,4,5)); // 年龄= 1,2,3,4,5
        SqlHelper<SysUser> sqlHelper = SqlHelper.<SysUser>of(map);
        List<SysUserVO> vos1 = baseService.listByDTO(sqlHelper); // 通过sqlHelper作为参数传入DynamicService进行查询
    
        // 其他条件1
        SqlHelper<Object> otherSqlHelper1= new SqlHelper<>();
        // 其他条件2
        SqlHelper<Object> otherSqlHelper2= new SqlHelper<>();
        // 设置条件......
        
        
        // 链式表达
        List<SysUserVO> list = SqlHelper.<SysUser>of()
                .eq(SysUser::getAge, 18)  // 年龄= 18
                .or() // 使下一个条件关系在当前层级的关系为或者 (年龄= 18 或 姓名!= 张三)
                .ne(SysUser::getName, "张三") // 姓名!= 张三
                .requiredNext() // 必须满足后面的条件(原理切换层级, 为将已添加的条件设置为子条件, 新条件设置为父条件)
                .like(SysUser::getName, "张") // 姓名模糊匹配 张
                .in(SysUser::getAge, Arrays.asList(1, 2, 3, 4, 5)) // 年龄= 1,2,3,4,5
                .notIn(SysUser::getAge, Arrays.asList(1, 2, 3, 4, 5)) // 年龄!= 1,2,3,4,5
                .with(otherSqlHelper1) // 包装另一个sqlHelper的所有条件
                .withChild(otherSqlHelper2) // 将另一个sqlHelper的所有条件作为子条件添加
                .wrap(baseService)// 指定DynamicService, 指定后的list, one, page方法会根据已设置的参数查询对应数据
                .list() // 查询列表
                //.one() // 查询一条数据
                //.page(1L, 10L) // 查询分页数据
                ;
    }

```

### 前端参数说明
#### 条件参数`conditions`
* `or`表示与其他条件的关系是否为`或者`(非必填,默认false)
* `field`表示`属性`名
* `operator`表示运算符(默认为`=`, 为`=`时无需传递, 不区分大小写)
* `value`表示属性对应的`值`

tips:  
* `operator`不区分大小写, 支持 `=`、`!=`、`>`、`>=`、`<`、`<=`、`in`、`not in` 、`like`、`not like`、`is null`、`is not null`   
* `value`对应的`operator`若为`in`或`not in`时,`value`需要为`["value1","value2","value3"]`的多参数形式   
* `value`对应的`operator`若为`is null`或`is not null`时,`value`可传递不为`null`的任意值   

#### 条件参数`child`
* 该参数实际为嵌套的子条件, 支持多重嵌套
* 内部嵌套的实际参数为`conditions`和`子child`

#### 排序参数`sorts`
* `field`表示`属性`名
* `desc`表示是否倒序(默认为`false`, 正序时无需传递)

#### 前端参数基础格式示例:
```json
{
  "conditions": [
    {
      "or": true, //或条件(非必填,默认false)
      "field": "", //属性名
      "operator": "", //运算符(非必填,默认=,可选值:=,>,<,!=,<>,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN)
      "value": {} //值(多个值时,数据为集合)
    },
    {
      "field": "name", //属性名示例
      "value": "zhangsan" //值示例
    },
    {
      "field": "name", //属性名示例
      "operator": "in", //多值运算符示例
      "value": ["zhangsan", "lisi", "wangwu"] //多值示例
    }
  ],
  "child": {
    "conditions": [
      {
        "or": true, //或条件(非必填,默认false)
        "field": "", //属性名
        "operator": "", //运算符(非必填,默认=,可选值:=,>,<,!=,<>,>=,<=,LIKE,NOT LIKE,IS NULL,IS NOT NULL,IN,NOT IN)
        "value": {} //值(多个值时,数据为集合)
      }
    ], //查询条件列表
    "child": {} //子条件
  },
  "sorts": [
    {
      "field": "", //属性名
      "desc": true //是否倒序(默认否, 为否时无需填写)
    }
  ]
}
```

#### 动态`sql`和对应`传参`示例
```sql
SELECT * FROM sys_user 
WHERE
age > 3 AND name LIKE '%张%'  # 父条件
AND ( id = 1 OR id = 2 ) # 子条件 
ORDER BY 
age DESC, id ASC
```
```json
{
  "conditions": [
    {
      "field": "age",
      "operator": ">",
      "value": 3
    },
    {
      "field": "name",
      "operator": "like",
      "value": "张"
    }
  ],
  "sorts": [
    {
      "field": "age",
      "desc": true
    },
    {
      "field": "id"
    }
  ],
  "child": {
    "conditions": [
      {
        "field": "id",
        "operator": "=",
        "value": 1
      },
      {
        "or": true,
        "field": "id",
        "operator": "=",
        "value": 2
      }
    ]
  }
}
```

## `DynamicService<T, V>`
该接口针对`IService`定义了一系列增强方法, 其中`T`为数据库实体类, `V`为VO数据展示类  
该接口大多方法都提供了`默认实现`, 实际需要实现的仅有`doSelect()`方法
* `getVOClass()`获取VO数据展示类
* `toEntity()`将指定对象转化为`T`
* `toVO()`将指定对象转化为`V`
* `toId()`获取`T`对象的`主键id`
* `insertByDTO()`新增方法, 默认返回值`R`为新增数据的`主键id`
* `updateByDTO()`更新方法
* `doSelect()`查询逻辑封装方法
* `oneById()`根据`主键id`查询单个VO
* `oneByDTO()`查询单个VO
* `listByDTO()`查询VO列表
* `pageByDTO()`查询VO分页
* `excelTemplate()`excel导入模板
* `excelImport()`excel文件导入
* `excelExport()`excel文件导出
* `lambdaHelper()`获取链式动态条件构造器(见`SqlHelper`), 使用方式类似`lambdaQuery()`

源码及实现:
* [DynamicService.java](src/main/java/io/github/bootystar/mybatisplus/enhance/core/DynamicService.java)
* [DynamicSqlServiceImpl.java](src/main/java/io/github/bootystar/mybatisplus/enhance/core/impl/DynamicSqlServiceImpl.java)
* [DynamicFieldServiceImpl.java](src/main/java/io/github/bootystar/mybatisplus/enhance/core/impl/DynamicFieldServiceImpl.java)


## DynamicMapper<T, V, S>
该接口定义了动态mapper的入参查询,其中`T`为数据库实体类, `V`为VO数据展示类, `S`为查询入参类
* 子mapper继承该类, 即可运行, 无需实现方法(需要提供对应xml文件)
* 该mapper的参数及对应`xml`文件会由生成器自动生成
* 可在`mapper.xml`文件中添加对应的额外表及字段检索等自定义逻辑

### xml中额外SQL编写
* 在`xml`文件中, 可根据自身需要进行连表或者字段检索
* 基础表别名固定为`a`, 请勿修改
* `selectFragment`为自动映射封装的查询条件
* `selectFragment`下方添加额外条件(添加条件时不需要添加`WHERE`关键字)
* `selectFragment`下处添加额外条件时, 建议始终添加`AND`或`OR`连接符, 系统会自动去除多余的连接符
* 无法自动映射的查询条件会统一存放到`param1.map`中, 可通过param1.map.xxx判断参数是否存在,并添加对应逻辑
* 无法自动映射的查询条件值为`null`时, 系统会将字符串`"null"`作为值添加到map中,避免`<if test"param1.map.xxx!=null">`判断失效
* `sortFragment`为自动映射封装的排序条件
* `sortFragment`下方可添加额外排序条件(添加条件时不需要添加`ORDER BY`关键字)
* 参数映射顺序`实体类属性字段信息`->`@TableFiled注解`->`DynamicEntity映射`

#### 默认生成的xml
```xml
<select id="listByDTO" resultType="io.github.bootystar.vo.SysUserVO">
    SELECT
    a.*
    FROM
    sys_user a
    <trim prefix="WHERE" prefixOverrides="AND|OR" suffixOverrides="AND|OR">
        <include refid="selectFragment"/>
    </trim>
    <trim prefix="ORDER BY" suffixOverrides=",">
        <include refid="sortFragment"/>
    </trim>
</select>
```

#### 自定义后的xml
```xml
<select id="listByDTO" resultType="io.github.bootystar.vo.SysUserVO">
    SELECT
    a.*
    FROM
    sys_user a
    <!--额外添加连表-->
    left join sys_role b on a.role_id = b.id
    <trim prefix="WHERE" prefixOverrides="AND|OR" suffixOverrides="AND|OR">
        <include refid="selectFragment"/>
        <!--在selectFragment下添加额外的查询条件-->
        <!--注意:记得使用AND|OR连接符,当映射条件不存在时会自动删除AND|OR符号-->
        AND a.deleted = 0 AND b.level = #{param1.map.roleLevel}
        <!--对未自动映射的条件进行判断, 并操作-->
        <if test="param1.map.xxx!=null">
            AND a.name = #{param1.map.xxx}
        </if>
    </trim>
    <trim prefix="ORDER BY" suffixOverrides=",">
        <include refid="sortFragment"/>
        <!--在sortFragment下额外添加排序-->
        a.create_time DESC , a.id DESC ,
    </trim>
</select>
```

#### 自动映射非本实体的表字段
* 通过在属性上添加`@TableField`注解指定映射, 指定`value`为`表名.字段名`或`表别名.字段名`指定其他表字段
* 通过实现`DynamicEntity`接口, 重写`extraFieldColumnMap()`方法指定映射

```java
public class SysUser implements DynamicEntity {

    // 指定roleLevel对应的字段为b表的level字段, 并注明该字段在本表中不存在
    @TableField(exist = false, value = "b.level")
    private String roleLevel;
    
    
    @Override
    public Map<String, String> extraFieldColumnMap() {
        HashMap<String, String> map = new HashMap<>();
        /*
        select a.* from
        sys_user a
        left join sys_role b on a.role_id = b.id
        left join sys_department on a.department_id = sys_department.id
         */
        // 指定roleLevel, 对应为b表(sys_role)的level
        map.put("roleLevel", "b.level");
        // departmentName, 对应为sys_department表的name
        map.put("departmentName", "sys_department.name");
        return map;
    }
}
```