# mybatis-plus-enhancer

[![Maven Central](https://img.shields.io/maven-central/v/io.github.bootystar/mybatis-plus-enhancer)](https://mvnrepository.com/artifact/io.github.bootystar/mybatis-plus-enhancer)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/github/stars/bootystar/mybatis-plus-enhancer?style=social)](https://github.com/bootystar/mybatis-plus-enhancer)

[English](README.md) | 中文

MyBatis-Plus 增强工具包，提供动态SQL构建、IService和BaseMapper增强功能，以及Excel导入导出支持。

## 功能特性

- **动态SQL构建**：通过[SqlHelper](src/main/java/io/github/bootystar/mybatisplus/enhancer/query/helper/SqlHelper.java)实现链式条件拼接
- **IService增强**：通过[EnhancedService](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedService.java)提供`voById`、`voList`、`voPage`等方法
- **BaseMapper增强**：通过[EnhancedMapper](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedMapper.java)提供动态查询接口
- **Excel导入导出**：集成FastExcel实现VO列表与Excel文件的转换
- **XML工具生成**：通过[MapperUtil](src/main/java/io/github/bootystar/mybatisplus/enhancer/util/MapperUtil.java)自动生成Mapper XML内容

## 仓库地址

- GitHub: https://github.com/bootystar/mybatis-plus-enhancer
- Maven Central: https://central.sonatype.com/artifact/io.github.bootystar/mybatis-plus-enhancer

## 安装

```xml
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
    <version>latest</version>
</dependency>
```

当前最新版本为: [![Maven Central](https://img.shields.io/maven-central/v/io.github.bootystar/mybatis-plus-enhancer)](https://mvnrepository.com/artifact/io.github.bootystar/mybatis-plus-enhancer)

## 快速开始

### 1. 假设已有mybatis-plus实体类和mapper

```java
@TableName("sys_user")
public class SysUser {
    @TableId(type = IdType.AUTO)
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```
```java
public interface SysUserMapper extends BaseMapper<SysUser> {
    
}
```

### 2. 创建VO类, 修改mapper接口
* VO视图类可直接使用实体类, 也可继承自实体类
* 使mapper接口继承[EnhancedMapper.java](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedMapper.java), 并指定泛型为VO类
* (可选)若有service层, 可使service实现[EnhancedService.java](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedService.java)接口, 即可拥有mapper对应所有方法

```java
public class SysUserVO {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```
```java
public interface SysUserMapper extends BaseMapper<SysUser>,EnhancedMapper<SysUserVO> {
    
}
```

### 3. 获取mapper.xml内容, 并将其复制到Mapper对应xml文件中


```java
import io.github.bootystar.mybatisplus.enhancer.util.MapperUtil;

public static void main(String[] args) {
    var mapperContent = MapperUtil.getMapperContent(SysUserMapper.class);
    System.out.println(mapperContent);
}
```
```xml
<select id="voQueryByXml" resultType="com.example.test.vo.SysUserVO">
    SELECT a.* FROM sys_user a
    <where>
        <include refid="io.github.bootystar.mybatisplus.enhancer.EnhancedMapper.dynamicSelect"/>
        AND a.deleted = 0
        <if test="param1.unmapped.nameLike!=null">
            AND a.name LIKE #{param1.un.nameLike}
        </if>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="io.github.bootystar.mybatisplus.enhancer.EnhancedMapper.dynamicSort"/>
        , a.create_time DESC, a.id DESC
    </trim>
</select>
```


### 4. 使用示例

```java
@RestController
@RequestMapping("/user")
public class SysUserController {
    
    @Autowired
    private SysUserService sysUserService;
    
    // 根据ID查询VO
    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserService.voById(id);
    }
    
    // 条件查询VO列表
    @GetMapping("/list")
    public List<SysUserVO> getUserList() {
        SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
            .ge(SysUser::getAge, 18)
            .like(SysUser::getName, "张");
        return helper.wrap(sysUserService).list();
    }
    
    // 分页查询VO
    @GetMapping("/page")
    public IPage<SysUserVO> getUserPage(@RequestParam(defaultValue = "1") Long current,
                                       @RequestParam(defaultValue = "10") Long size) {
        SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
            .ge(SysUser::getAge, 18);
        return helper.wrap(sysUserService).page(current, size);
    }
}
```

## 核心组件

### SqlHelper - 动态SQL构建器

[SqlHelper](src/main/java/io/github/bootystar/mybatisplus/enhancer/query/helper/SqlHelper.java)是动态SQL构建的核心组件，支持链式调用和Lambda表达式。

```java
// 基本用法
SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
    .eq(SysUser::getName, "张三")
    .ge(SysUser::getAge, 18)
    .orderByDesc(SysUser::getCreateTime);

// OR条件
SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
    .eq(SysUser::getName, "张三")
    .or(h -> h.eq(SysUser::getName, "李四")
              .eq(SysUser::getAge, 20));

// 使用wrap方法简化调用
List<SysUserVO> list = helper.wrap(userService).list();
```

### EnhancedService - 增强服务接口

[EnhancedService](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedService.java)提供了丰富的VO查询方法：

- `voById(Serializable id)`: 根据ID查询单个VO
- `voByDTO(Object s)`: 根据DTO查询单个VO
- `voList(Object s)`: 根据条件查询VO列表
- `voPage(Object s, Long current, Long size)`: 分页查询VO

### EnhancedMapper - 增强Mapper接口

[EnhancedMapper](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedMapper.java)提供了基于XML的动态查询功能。

### JSON参数查询

支持通过JSON格式参数构建查询条件，详情请参考[JSON参数说明文档](JSON_PARAMETER_ZH.md)。

## 配置

### application.yml

```yaml
mybatis-plus:
  configuration:
    log-impl: org.apache.ibatis.logging.stdout.StdOutImpl
logging:
  level:
    org.apache.ibatis: debug
    java.sql: debug
```

## 更多示例

请参考[测试用例](src/test/java/com/example)获取更多使用示例。

## 许可证

[Apache License 2.0](LICENSE)