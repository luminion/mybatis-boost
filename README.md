# mybatis-plus-enhancer

[![Maven Central](https://img.shields.io/maven-central/v/io.github.bootystar/mybatis-plus-enhancer)](https://mvnrepository.com/artifact/io.github.bootystar/mybatis-plus-enhancer)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/github/stars/bootystar/mybatis-plus-enhancer?style=social)](https://github.com/bootystar/mybatis-plus-enhancer)

English | [中文](README_ZH.md)

An enhancement toolkit for MyBatis-Plus that provides dynamic SQL building, IService and BaseMapper enhancements, and Excel import/export support.

## Features

- **Dynamic SQL Building**: Chain condition splicing through [SqlHelper](src/main/java/io/github/bootystar/mybatisplus/enhancer/query/helper/SqlHelper.java)
- **IService Enhancement**: Provides methods like `voById`, `voList`, `voPage` through [EnhancedService](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedService.java)
- **BaseMapper Enhancement**: Provides dynamic query interface through [EnhancedMapper](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedMapper.java)
- **Excel Import/Export**: Integration with FastExcel to convert VO lists to Excel files
- **XML Tool Generation**: Automatically generate Mapper XML content through [MapperUtil](src/main/java/io/github/bootystar/mybatisplus/enhancer/util/MapperUtil.java)

## Repository

- GitHub: https://github.com/bootystar/mybatis-plus-enhancer
- Maven Central: https://central.sonatype.com/artifact/io.github.bootystar/mybatis-plus-enhancer

## Installation

```xml
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
    <version>latest</version>
</dependency>
```

Current latest version: [![Maven Central](https://img.shields.io/maven-central/v/io.github.bootystar/mybatis-plus-enhancer)](https://mvnrepository.com/artifact/io.github.bootystar/mybatis-plus-enhancer)

## Quick Start

### 1. Create Entity Class

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

### 2. Create VO Class

```java
public class SysUserVO {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```

### 3. Create Mapper Interface

```java
@Mapper
public interface SysUserMapper extends EnhancedMapper<SysUserVO> {
    // Custom methods can be added
}
```

### 4. Create Service Interface

```java
public interface SysUserService extends EnhancedService<SysUserVO> {
    // Custom methods can be added
}
```

### 5. Create Service Implementation

```java
@Service
public class SysUserServiceImpl extends ServiceImpl<SysUserMapper, SysUser> implements SysUserService {
    // Custom implementations can be added
}
```

### 6. Usage Examples

```java
@RestController
@RequestMapping("/user")
public class SysUserController {
    
    @Autowired
    private SysUserService sysUserService;
    
    // Query VO by ID
    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserService.voById(id);
    }
    
    // Query VO list with conditions
    @GetMapping("/list")
    public List<SysUserVO> getUserList() {
        SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
            .ge(SysUser::getAge, 18)
            .like(SysUser::getName, "John");
        return helper.wrap(sysUserService).list();
    }
    
    // Page query VO
    @GetMapping("/page")
    public IPage<SysUserVO> getUserPage(@RequestParam(defaultValue = "1") Long current,
                                       @RequestParam(defaultValue = "10") Long size) {
        SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
            .ge(SysUser::getAge, 18);
        return helper.wrap(sysUserService).page(current, size);
    }
}
```

## Core Components

### SqlHelper - Dynamic SQL Builder

[SqlHelper](src/main/java/io/github/bootystar/mybatisplus/enhancer/query/helper/SqlHelper.java) is the core component for dynamic SQL building, supporting chain calls and Lambda expressions.

```java
// Basic usage
SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
    .eq(SysUser::getName, "John")
    .ge(SysUser::getAge, 18)
    .orderByDesc(SysUser::getCreateTime);

// OR conditions
SqlHelper<SysUser> helper = SqlHelper.of(SysUser.class)
    .eq(SysUser::getName, "John")
    .or(h -> h.eq(SysUser::getName, "Jane")
              .eq(SysUser::getAge, 20));

// Use wrap method to simplify calls
List<SysUserVO> list = helper.wrap(userService).list();
```

### EnhancedService - Enhanced Service Interface

[EnhancedService](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedService.java) provides rich VO query methods:

- `voById(Serializable id)`: Query a single VO by ID
- `voByDTO(Object s)`: Query a single VO by DTO
- `voList(Object s)`: Query VO list by conditions
- `voPage(Object s, Long current, Long size)`: Page query VO

### EnhancedMapper - Enhanced Mapper Interface

[EnhancedMapper](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedMapper.java) provides XML-based dynamic query functionality.

### JSON Parameter Query

Supports building query conditions through JSON format parameters. For details, please refer to [JSON Parameter Documentation](JSON_PARAMETER.md).

## Configuration

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

## More Examples

Please refer to [Test Cases](src/test/java/com/example) for more usage examples.

## License

[Apache License 2.0](LICENSE)