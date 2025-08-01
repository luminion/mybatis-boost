# mybatis-plus-enhancer

[![Maven Central](https://img.shields.io/maven-central/v/io.github.bootystar/mybatis-plus-enhancer)](https://mvnrepository.com/artifact/io.github.bootystar/mybatis-plus-enhancer)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/github/stars/bootystar/mybatis-plus-enhancer?style=social)](https://github.com/bootystar/mybatis-plus-enhancer)

[English](README.md) | 中文

MyBatis-Plus 增强工具包，提供动态SQL构建、后缀映射查询、IService和BaseMapper增强功能，以及Excel导入导出支持。

## 功能特性

- **后缀SQL构建**：支持`字段`+`后缀`自动映射不同类型查询
- **动态SQL构建**：支持根据入参动态拼接条件
- **联表属性查询**：支持非本表字段的查询自动映射
- **Map查询条件**：自动转化Map参数
- **数据字段映射**：自动转换属性为数据库字段
- **SQL反注入**：通过预编译SQL, 防止SQL注入
- **Lambda链式调用**：支持链式调用追加参数条件
- **VO类型转化**：自动将查询结果转化为指定类
- **BaseMapper增强**：添加`voById`、`voList`、`voPage`等方法
- **IService查询增强**：添加`voById`、`voList`、`voPage` 等方法
- **IService业务增强**：添加`insertByDTO`、`updateByDTO`等方法
- **IService集成Excel**：集成`FastExcel`和`EasyExcel`, 支持Excel导入/导出


## 仓库地址

- GitHub: https://github.com/bootystar/mybatis-plus-enhancer
- Maven Central: https://central.sonatype.com/artifact/io.github.bootystar/mybatis-plus-enhancer

## maven依赖
当前最新版本为:  
[![Maven Central](https://img.shields.io/maven-central/v/io.github.bootystar/mybatis-plus-enhancer)](https://mvnrepository.com/artifact/io.github.bootystar/mybatis-plus-enhancer)
```xml
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
    <version>latest</version>
</dependency>
```

## 快速开始

### 1. 创建mybatis-plus实体类和BaseMapper 
若已有mybatis-plus实体类和BaseMapper, 可跳过此步骤

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

### 2. 扩展mapper接口
* 创建或指定`VO类`, 用于展示查询结果
* 使`mapper`接口继承[EnhancedMapper](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedMapper.java), 并指定泛型为VO类
* 通过`工具类`获取`mapper.xml`内容, 并将其复制到对应xml文件中
* (可选) 若有service层, 可使service实现[EnhancedService](src/main/java/io/github/bootystar/mybatisplus/enhancer/EnhancedService.java)接口, 即可拥有mapper对应所有方法


```java
// 用于封装查询结果的VO类, 可以继承自实体类, 也可以直接使用实体类
public class SysUserVO {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```
```java
// mapper接口文件, 使其继承EnhancedMapper接口
public interface SysUserMapper extends BaseMapper<SysUser>, 
        EnhancedMapper<SysUserVO> {
}
```

```java
import io.github.bootystar.mybatisplus.enhancer.util.MapperUtil;

// 通过工具类获取mapper.xml文件的sql片段
public static void main(String[] args) {
    var mapperContent = MapperUtil.getMapperContent(SysUserMapper.class);
    System.out.println(mapperContent);
}
```
```xml
<!--复制工具类生成的该sql片段到mapper.xml文件中-->
<select id="voQueryByXml" resultType="com.example.test.vo.SysUserVO">
    SELECT a.* FROM sys_user a
    <where>
        <include refid="io.github.bootystar.mybatisplus.enhancer.EnhancedMapper.dynamicSelect"/>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="io.github.bootystar.mybatisplus.enhancer.EnhancedMapper.dynamicSort"/>
    </trim>
</select>
```

### 3. 使用示例

```java
import io.github.bootystar.mybatisplus.enhancer.query.helper.SqlHelper;
import java.util.List;
import java.util.Map;

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

    // 通过DTO对象查询
    @PostMapping("/dto")
    public List<SysUserVO> getUsersByDTO(@RequestBody SysUserDTO dto) {
        return sysUserService.voList(dto);
    }

    // 通过map条件查询(支持后缀映射不同类型查询)
    @PostMapping("/map")
    public List<SysUserVO> getUsersByMap(@RequestBody Map<String, Object> params) {
        return sysUserService.voList(params);
    }

    // 入参拼装动态sql查询
    @PostMapping("/sql")
    public List<SysUserVO> getUsersBySql(@RequestBody SqlHelper<SysUser> sqlHelper) {
        return sysUserService.voList(sqlHelper);
    }

    // lambda调用,封装必须条件
    @PostMapping("/lambda")
    public List<SysUserVO> getUsersBySql(@RequestBody Map<String, Object> params) {
        return SqlHelper.of(SysUser.class)
                .with(params) // 添加参数, 支持实体类, DTO对象, map, SqlHelper等
                .eq(SysUser::getState,1) // state=1
                .ge(SysUser::getAge, 18) // age>=18
                .like(SysUser::getUserName, "tom") // userName like '%tom%'
                .wrap(sysUserService)
                .voList();
    }

    // 分页查询
    @PostMapping("/page/{current}/{size}")
    public IPage<SysUserVO> getUserPage(@RequestBody Map<String, Object> params,
                                        @PathVariable("current") Long current,
                                        @PathVariable("size") Long size) {
        return sysUserService.voPage(params, current, size);
    }

    // Excel导入
    @PostMapping("/excel/import")
    public int importExcel(@RequestParam("file") MultipartFile file) {
        // 返回导入条数
        return sysUserService.importExcel(file, SysUserVO.class);
        ;
    }

    // Excel导出
    @PostMapping("/excel/export/{current}/{size}")
    public void exportExcel(@RequestBody Map<String, Object> params,
                            @PathVariable("current") Long current,
                            @PathVariable("size") Long size) {
        sysUserService.exportExcel(fileName, SysUserVO.class);
    }


}
```
java代码使用方式请参考:[示例](src/test/java/com/example)

## 核心功能

### 后缀查询
- 前端可以在传入参数中添加`字段后缀`轻松实现各种查询需求
- 前端入参在不添加后缀时, 等同于`等于`查询
- 后端可用`实体类`或`Map`接收参数

#### 前端入参示例
原始字段
```json
{
  "name": "mike",
  "version": 1,
  "age": 18,
  "state": 1
}
```

后缀示例: 
- `name`包含`mike`
- `version`为`1`
- `age`在`18-60`之间
- `state`为`1`或`2`或`3` 

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGe": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

支持的后缀关键字：
- `Ne` - 不等于
- `Lt` - 小于
- `Le` - 小于等于
- `Gt` - 大于
- `Ge` - 大于等于
- `Like` - 模糊匹配
- `NotLike` - 反模糊匹配
- `In` - IN查询
- `NotIn` - NOT IN查询
- `IsNull` - IS NULL
- `IsNotNull` - IS NOT NULL
- `bitWith` - 位运算, 包含指定bit位
- `BitWithout` - 位运算, 不包含指定bit位

### 动态SQL

- 前端可以自由指定需要查询的`字段`和`值`, 并自由指定查询类型, 拼接, 排序, 组合多条件
- 后端使用`SqlHelper`对象接收参数

#### 入参示例

原始字段:
```json
{
  "name": "mike",
  "version": 1,
  "age": 18,
  "state": 1
}
```
#### 一般条件拼接
- 通过`conditions`字段指定查询条件,
- 其中每个条件对象`field`表示字段,`value`表示值,`operator`表示操作符号
- `operator`不填写时,默认为等于, 可选值：
  - `=` - 等于(默认),
  - `<>` - 不等于
  - `>` - 大于
  - `>=` - 大于等于
  - `<` - 小于
  - `<=` - 小于等于
  - `LIKE` - 模糊匹配
  - `NOT LIKE` - 反模糊匹配
  - `IN` - IN查询
  - `NOT IN` - NOT IN查询
  - `IS NULL` - 指定字段为NULL
  - `IS NOT NULL` - 指定字段不为NULL
  - `$>` - 位运算, 包含指定bit位
  - `$=` - 位运算, 不包含指定bit位

查询`name`为`mike`, `version`为`1`, `state`为`1`或`2`或`3`的数据
```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "value": 1
    },
    {
      "field": "state",
      "operator": "IN",
      "value": [1, 2, 3]
    }
  ]
}
```
#### 指定排序的查询
- 通过`sorts`字段指定排序字段, 
- 其中每个条件对象`field`表示排序的字段,`isDesc`表示是否倒序

查询`name`为`mike`, `version`为`1`的数据, 并将结果按照`id`降序, `age`升序排列
```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "value": 1
    }
  ],
  "sorts": [
    {
      "field": "id",
      "isDesc": true
    },
    {
      "field": "age"
    }
  ]
}
```