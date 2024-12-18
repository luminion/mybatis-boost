# mybatis-plus-enhancer
a enhancer of mybatis-plus


# maven依赖

## 引入依赖

使用`dependencyManagement`管理依赖,避免版本冲突
```xml
<dependencyManagement>
    <dependencies>
        <dependency>
            <groupId>io.github.bootystar</groupId>
            <artifactId>mybatis-plus-enhancer</artifactId>
            <version>版本号</version>
            <type>pom</type>
            <scope>import</scope>
        </dependency>
    </dependencies>
</dependencyManagement>
```
按需引入相关依赖
```xml
<!--mybatis-plus-enhancer-->
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
</dependency>

<!-- spring boot3 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-spring-boot3-starter</artifactId>
</dependency>

<!-- spring boot2 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-boot-starter</artifactId>
</dependency>


<!-- jdk 11+ 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-jsqlparser</artifactId>
</dependency>

<!-- jdk 8+ 引入可选模块 -->
<dependency>
    <groupId>com.baomidou</groupId>
    <artifactId>mybatis-plus-jsqlparser-4.9</artifactId>
</dependency>
```
正式版本中央仓库地址  
中央仓库同步到阿里云有延迟, 若阿里云无法拉取, 可通过配置拉取
```xml
<repositories>
    <repository>
        <id>release</id>
        <name>release</name>
        <url>https://s01.oss.sonatype.org/content/repositories/releases/</url>
    </repository>
</repositories>
```


## SNAPSHOT快照版本
若需引入快照版本, 需配置快照仓库地址
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
若使用阿里云仓库, 需在maven的`settings.xml`文件中配置`!snapshots`以便拉取
```xml
<mirror>
  <id>aliyunmaven</id>
  <mirrorOf>*,!snapshots</mirrorOf>
  <name>aliyun</name>
  <url>https://maven.aliyun.com/repository/public</url>
</mirror>
```

# 代码生成器
## 生成代码
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

## 可选配置项
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
            custom
                // 文件相关
                .enableFileOverride() // 文件覆盖生成(DTO、VO)
                .enableSwaggerModelWithAnnotation() // 启用swagger/springdoc参数类注解(默认关闭,避免swagger同名冲突)
                .enableSwaggerAnnotationWithUUID() // 启用swagger/springdoc文档额外uuid标识(开启参数类注解时避免swagger同名冲突)
                .class4SelectDTO(Map.class) // 使用指定类作为查询入参DTO(推荐使用Map或SqlHelper)
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
                .disablePostQuery() // 复杂查询不使用post请求
                // mapper额外生成项
                .sortColumnClear() // 清空排序
                .sortColumn("create_time",true) // 添加排序(字段,是否倒序)
                .sortColumn("id",true) // 添加排序(字段,是否倒序)
                // 需要生成的方法
                .disableInsert() // 不生成新增
                .disableUpdate() // 不生成更新
                .disableDelete() // 不生成删除
                .disableSelect() // 不生成查询(若生成器为额外代码生成器并生成了导出, 则此项无效)
                .disableImport() // 不生成导入
                .disableExport() // 不生成导出
                // 特殊项, 因不同生成器而异
                .disableOverrideMethods() // 不生成重写的父类方法(动态字段生成器/动态sql生成器)
                .fieldSuffixBuilder(builder -> { // 额外自定义字段后缀(额外代码生成器/动态字段生成器)
                // 该项默认无需配置, 配置后, 只会根据已配置的字段生成额外后缀, 未配置的类型不会生成后缀
                                builder
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
                    ;})// 额外自定义字段后缀(额外代码生成器/动态字段生成器)
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
## 生成代码(非lambda链式调用)

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
generator.getCustomConfigBuilder().getFieldSuffixBuilder()// 自定义字段后缀配置器
        .ne("Ne") // ...略
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

# 运行时增强

## service接口 
### DynamicService<T, V>
该接口定义了动态服务的一系列增强方法, 其中`T`为数据库实体类, `V`为VO数据展示类  
继承该接口并指定泛型即可使用下述方法
* `getVoClass()`获取VO数据展示类
* `toEntity()`将指定对象转化为数据库实体类对象
* `toVO()`将指定对象转化为VO数据展示类对象
* `toId()`将指定对象转化为主键值
* `insertByDTO()`新增方法, 默认返回值R为新增数据的实际主键(重写时可搭配`toId()`使用)
* `updateByDTO()`更新方法
* `doSelect()`查询逻辑封装方法
* `oneById()`根据id查询单个VO
* `oneByDTO()`查询单个VO
* `listByDTO()`查询VO列表
* `pageByDTO()`查询VO分页
* `excelTemplate()`excel导入模板
* `excelImport()`excel文件导入
* `excelExport()`excel文件导出
* `lambdaHelper()`获取链式动态条件构造器(见`SqlHelper`), 使用方式类似mybatis-plus中的`lambdaQuery()`

```java
public interface SysUserService extends DynamicService<SysUser, SysUserVO> {

}
```
### DynamicMapper<T, V, S>
该接口定义了动态mapper的入参查询,其中`T`为数据库实体类, `V`为VO数据展示类, `S`为查询入参类
* 子mapper继承该类, 即可运行, 无需实现
* 该mapper的参数及对应`xml`文件会由生成器自动生成
* 所有的增强查询都会最终通过`listByDTO()`从数据库查询
* 可在`mapper.xml`文件中添加对应的额外表及字段检索等自定义逻辑

```java
public interface SysUserMapper extends DynamicMapper<SysUser, SysUserVO, Object> {

}
```
#### xml中额外SQL编写
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

##### 默认生成的xml
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

##### 自定义后的xml
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
##### 映射非本实体的表字段
* 通过在属性上添加`@TableField`注解指定映射, 指定`value`为`表名.字段名`或`表别名.字段名`指定其他表字段
* 通过实现`DynamicEntity`接口, 重写`extraFieldColumnMap()`方法指定映射

```java
    // 指定roleLevel对应的字段为b表的level字段, 并注明该字段在本表中不存在
    @TableField(exist = false, value = "b.level")
    private String roleLevel;
```

### DynamicEntity

该接口定义了可被动态sql增强的实体类

* 在`xml`中添加需要连接的表
* 通过`extraFieldColumnMap()`指定额外的属性名->数据库字段名映射
* 指定的逻辑删除字段无效, 会被过滤, 防止侵入
* 可通过`表名.字段名`或`表别名.字段名`指定其他表字段

```java
public class SysUser implements DynamicEntity {
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

## service接口实现

### DynamicFieldServiceImpl<M, T, V>
动态字段服务实现, 通过动态字段及动态sql实现嵌套增强
* 继承该类, 指定`Mapper类`,`实体类`, `VO类`
* 支持`SqlHelper`,`Map`,`一般类(会通过反射获取属性与值)`作为查询参数
* 通过重写`initSuffixBuilder()`方法配置字段后缀, 若不重写该方法, 会按照默认后缀匹配, 重写后仅匹配已指定的后缀
* 当入参为`一般实体类`或`Map`时, 会自动根据指定后缀追加相关的条件查询(例如`nameLike`字段,会转化为`name`对应的模糊查询)
```java
public class SysUserServiceImpl extends DynamicFieldServiceImpl<SysUserMapper, SysUser ,SysUserVO>{
    @Override
    protected FieldSuffixBuilder initSuffixBuilder() {
        return new FieldSuffixBuilder()
                .like("_like") // 当字段名以_like结尾时, 会自动将该查询转化为like查询
                .ge("Ge") // 当字段名以Ge结尾时, 会自动将该查询转化为大于等于查询
                // ...
                ;
    }
}
```

### DynamicSqlServiceImpl<M, T, V>
动态sql服务实现, 通过动态sql嵌套增强
* 继承该类, 指定`Mapper类`,`实体类`, `VO类`
* 使用`SqlHelper`,`Map`,`一般类(会通过反射获取属性与值)`作为查询参数
* 会自动根据情况生成条件匹配sql
```java
public class SysUserServiceImpl extends DynamicSqlServiceImpl<SysUserMapper, SysUser ,SysUserVO>{
    
}
```
## SqlHelper<T>动态sql工具
该工具用于生成sql片段, 支持Object入参  
DynamicService默认通过该类生成动态sql    
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
