# SQL Booster

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)
[![License](https://img.shields.io/badge/license-Apache%202.0-blue.svg)](LICENSE)
[![GitHub](https://img.shields.io/github/stars/luminion/sql-booster?style=social)](https://github.com/luminion/sql-booster)

SQL Booster 是一个数据库查询设计的增强工具包，旨在简化和增强数据访问层的开发。提供了强大的动态SQL动态条件和后缀查询映射功能。

## 功能特性

- **后缀SQL构建**：支持`字段`+`后缀`自动映射不同类型查询
- **动态SQL构建**：支持根据入参动态拼接条件
- **Map查询条件**：自动转化Map参数
- **数据字段映射**：自动转换属性为数据库字段
- **SQL反注入**：通过预编译SQL, 防止SQL注入
- **Lambda链式调用**：支持链式调用追加参数条件
- **VO类型转化**：自动将查询结果转化为指定类
- **额外查询方法**：添加`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`、`voPage`等方法

---

## 包结构说明

```
io.github.luminion.sqlbooster
├── core                  # 核心的 SQL 构建器和引擎
│   ├── Page.java
│   ├── Booster.java
│   ├── BoosterCore.java
│   ├── BoosterEngine.java
│   └── MethodReference.java
├── util                  # 提供项目使用的各种工具类
│   ├── BoostUtils.java
│   ├── ExcelUtils.java
│   ├── MapperUtils.java
│   └── ReflectUtils.java
├── model                 # 定义了 API、SQL 和枚举等数据模型
│   ├── api
│   ├── sql
│   └── enums
├── config                # 提供 Spring Boot 的自动配置功能
│   └── BoosterAutoConfiguration.java
├── provider              # 包含各种提供者接口和实现，用于扩展和自定义
│   ├── support
│   ├── BoostProvider.java
│   ├── TableNameProvider.java
│   ├── IdPropertyProvider.java
│   ├── GetterPropertyProvider.java
│   └── PropertyToColumnAliasMapProvider.java
└── extension             # 提供了与 MyBatis、MyBatis-Plus 和 PageHelper 等第三方库的集成
    ├── mybatis
    ├── pagehelper
    └── mybatisplus
```

---

## Maven 依赖

[![Maven Central](https://img.shields.io/maven-central/v/io.github.luminion/sql-booster)](https://mvnrepository.com/artifact/io.github.luminion/sql-booster)

```xml
<dependency>
    <groupId>io.github.luminion</groupId>
    <artifactId>sql-booster</artifactId>
    <version>latest</version>
</dependency>
```

---

## 快速开始
[code-generator](https://github.com/luminion/code-generator)代码生成器已适配该框架, 可以一键生成代码, 开箱即用

### 1. 创建实体类
若已有实体类, 可忽略该步骤
```java
// 数据库对应的实体类
public class SysUser {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```
```java
// 用于封装查询结果的VO类, 可以继承自实体类, 也可以直接使用实体类
public class SysUserVO {
    private Long id;
    private String name;
    private Integer age;
    // getter/setter...
}
```
### 2. 扩展mapper接口

获取xml文件内容
```java
import io.github.luminion.sqlbooster.util.MapperUtils;

public static void main(String[] args) {
    String mapperContent = MapperUtils.getMapperContent(SysUser.class, SysUserVO.class);
    System.out.println(mapperContent);
}
```
将获取的内容粘贴到mapper.xml文件中, 并根据需要`连表`/`添加条件`/`添加排序`
```xml
<!--复制工具类生成的该sql片段到mapper.xml文件中-->
<select id="selectByWrapper" resultType="com.example.test.vo.SysUserVO">
    SELECT 
    a.* 
    FROM 
    sys_user a
    <where>
        <include refid="sqlbooster.conditions"/>
        <!--此处编写自定义条件SQL, 未自动映射的条件可通过param1.extra获取, 编写时以AND开头以兼容自动映射的查询条件-->
        AND a.deleted = 0 
        <if test="param1.extra.userDeptName != null">
            AND a.dept_id in (SELECT id FROM sys_department WHERE name = #{param1.extra.userDeptName})
        </if>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="sqlbooster.sorts"/>
        <!--此处编写排序字段SQL, 编写时以,开头以兼容自动映射的排序-->
        , a.created_time DESC , a.id DESC
    </trim>
</select>
```

### 3. Mapper接口继承指定类
继承后可获得`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`、`voPage`等方法

提供以下几种继承, 任选其一
* 继承`BoosterEngine`, 无分页功能
* 继承`PageHelperBooster`, 完整功能, 使用`PageHelper`分页(需自行引入`PageHelper`依赖)
* 继承`MybatisPlusBooster`, 完整功能, 使用`IPage`分页(需自行引入`Mybatis-plus`依赖),


####  Mybatis环境, 使用`PageHelperBooster`
```java
import io.github.luminion.sqlbooster.extension.pagehelper.PageHelperBooster;

// 继承PageHelperBooster
public interface SysUserMapper extends PageHelperBooster<SysUser, SysUserVO>{

}
```
####  Mybatis-plus环境, 使用`MybatisPlusBooster`

针对Mybatis-plus环境, 提供了细分接口供`Service`/`ServiceImpl`/`Mapper`进行继承

- `BoosterMpMapper`继承了`BaseMapper`和`MybatisPlusBooster`
- `BoosterMpServiceImpl`继承`ServiceImpl`和`MybatisPlusBooster`
- `BoosterMpService`继承了`IService`和`MybatisPlusBooster`

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpMapper;

// 继承BoosterBaseMapper
// eg: BoosterBaseMapper已继承BaseMapper, SysUserMapper无需继承原BaseMapper
public interface SysUserMapper extends BoosterMpMapper<SysUser, SysUserVO> {

}
```

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpServiceImpl;

// 继承BoosterMpServiceImpl
// eg: BoosterMpServiceImpl已继承ServicImpl, SysUserServiceImpl无需继承原ServiceImpl
public class SysUserServiceImpl extends BoosterMpServiceImpl<SysUser, SysUserVO> {

}
```

```java
import io.github.luminion.sqlbooster.extension.mybatisplus.BoosterMpService;

// BoosterMpService
// eg: BoosterMpService已继承IService, SysUserService无需继承原IService
public class SysUserService extends BoosterMpService<SysUser, SysUserVO> {

}
```

#### 使用`BoosterEngine`, 不使用分页
* `BoosterEngine`提供了核心功能的多个默认实现, 但不包含分页功能
* `BoosterEngine`提供了`voById`、`voByIds`、`voFirst`、`voUnique`、`voList`等方法的实现
* `BoosterEngine`分页查询`voPage`方法在不调用时对业务逻辑无影响
* `BoosterEngine`分页查询`voPage`方法在调用时会抛出一个`UnsupportedOperationException`异常

```java
import io.github.luminion.sqlbooster.core.BoosterEngine;

// mapper继承BoosterEngine
public interface SysUserMapper extends BoosterEngine<SysUser, SysUserVO> {
    
}
```

#### 使用`BoosterEngine`, 并重写分页逻辑
* 默认该接口有4个不同参数的`voPage()`分页方法, 不使用分页功能时无需实现或重写
* 实际运行时会所有分页方法会最终重载到`voPage(Wrapper, long, long)`这个方法中
* 需要分页时, 仅重写`voPage(Wrapper, long, long)`方法, 添加分页的实现逻辑即可




建议抽象一个父接口书写逻辑, 继承`BoosterEngine`, 其他Mapper再继承该接口, 以免多次重写:

```java
import io.github.luminion.sqlbooster.core.BoosterEngine;

// 自定义全局接口, 继承BoosterEngine
public interface CustomBooster<T> extends BoosterEngine<SysUser, SysUserVO> {

    @Override
    default Page<SysUserVO> voPage(Wrapper<SysUser> wrapper, long pageNum, long pageSize) {
        // 查询预处理 - 提供给子类重写的方法, 可用于对wrapper进行预处理, 可以不调用, 但建议调用以规范行为
        voPreProcess(wrapper);
        
        // !!!重要!!!, 记得调用SqlHelper.process()方法
        // SqlHelper.process()方法用于处理动态映射和后缀映射, 同时检查条件合法性, 防止sql注入
        BaseHelper<T> sqlHelper = SqlHelper.of(wrapper).entity(this)
                .process(SuffixProcessor.of()::process);
        
        // 分页逻辑, 以下为Mybatis-plus的分页示例, 实际实现时替换为自己的即可
        PageDTO<V> pageInfo = new PageDTO<>(pageNum, pageSize);
        List<V> vs = selectByBooster(sqlHelper, pageInfo); // 真正执行查询的mapper层方法
        pageInfo.setRecords(vs);
        MybatisPlusPage<V> page = new MybatisPlusPage<>(pageInfo);


        // 查询后处理 - 提供给子类重写的方法, 可用于对查询结果进行后处理, 可以不调用, 但建议调用以规范行为
        voPostProcess(page.getRecords(), sqlHelper, page);
        return null;
    }
    
}
```

```java
// mapper继承自定义接口
public interface SysUserMapper extends CustomBooster<SysUser, SysUserVO> {

    
}
```

<br/>

---

## 使用示例

```java
import io.github.luminion.sqlbooster.model.sql.helper.SqlHelper;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/user")
public class SysUserController {

    // 此处引入Mapper接口或Service接口
    @Autowired
    private SysUserMapper sysUserMapper;

    // 根据ID查询VO
    @GetMapping("/{id}")
    public SysUserVO getUserById(@PathVariable Long id) {
        return sysUserMapper.voById(id);
    }

    // 通过DTO对象查询
    @PostMapping("/dto")
    public List<SysUserVO> getUsersByDTO(@RequestBody SysUserDTO dto) {
        return sysUserMapper.voList(dto);
    }

    // 通过map条件查询(支持后缀映射不同类型查询)
    @PostMapping("/map")
    public List<SysUserVO> getUsersByMap(@RequestBody Map<String, Object> params) {
        return sysUserMapper.voList(params);
    }

    // 使用SqlHelper作为参数时, 前端可通过入参动态指定条件及排序
    @PostMapping("/sql")
    public List<SysUserVO> getUsersBySql(@RequestBody SqlHelper<SysUser> sqlHelper) {
        return sysUserMapper.voList(sqlHelper);
    }

    // lambda调用,添加必要条件, 例如权限角色等
    @PostMapping("/lambda")
    public List<SysUserVO> getUsersBySql(@RequestBody Map<String, Object> params) {
        return SqlHelper.of(SysUser.class)
                .merge(params) // 合并或添加条件, 支持实体类, DTO对象, map, SqlHelper等
                .eq(SysUser::getState, 1) // state=1
                .ge(SysUser::getAge, 18) // age>=18
                .in(SysUser::getRoleId, Arrays.asList(1, 2))
                .like(SysUser::getUserName, "tom") // userName like '%tom%'
                .wrap(sysUserMapper)
                .voList();
    }

    // 分页查询
    @PostMapping("/page/{current}/{size}")
    public IPage<SysUserVO> getUserPage(@RequestBody Map<String, Object> params,
                                        @PathVariable("current") Long current,
                                        @PathVariable("size") Long size) {
        return sysUserMapper.voPage(params, current, size);
    }
    
    // 2025年11月
    // 因EasyExcel停更, 续作FastExcel变更为Apache fesod
    // Apache fesod暂未孵化, 待api稳定后, 再提供Excel相关功能
//    // Excel导出
//    @PostMapping("/excel/export/{current}/{size}")
//    public void exportExcel(@RequestBody Map<String, Object> params,
//                            @PathVariable("current") Long current,
//                            @PathVariable("size") Long size) {
//        sysUserMapper.exportExcel(fileName, SysUserVO.class);
//    }
}
```

---

## 核心功能

### 后缀动态映射
- 在`参数`名称后添加特殊的后缀, 可以`动态映射`为`不同类型`的查询
- 在不添加后缀时, 等同于`等于`查询
- 后端可用`实体类`或`Map`接收参数

#### 后缀映射表

| 后缀                           | 操作符           | 操作说明      | 示例 (JSON Key)                                           | 值类型                         |
|------------------------------|---------------|-----------|---------------------------------------------------------|-----------------------------|
| (无)                          | `=`           | 等于        | `"name": "mike"`                                        | String, Number, Boolean     |
| `Ne` / `_ne`                 | `<>`          | 不等于       | `"ageNe": 18` / `"age_ne": 18`                          | String, Number, Boolean     |
| `Lt` / `_lt`                 | `<`           | 小于        | `"ageLt": 18` / `"age_lt": 18`                          | Number, Date                |
| `Le` / `_le`                 | `<=`          | 小于等于      | `"ageLe": 18` / `"age_le": 18`                          | Number, Date                |
| `Gt` / `_gt`                 | `>`           | 大于        | `"ageGt": 18` / `"age_gt": 18`                          | Number, Date                |
| `Ge` / `_ge`                 | `>=`          | 大于等于      | `"ageGe": 18` / `"age_ge": 18`                          | Number, Date                |
| `Like` / `_like`             | `LIKE`        | 模糊匹配      | `"nameLike": "mike"` / `"name_like": "mike"`            | String                      |
| `NotLike` / `_not_like`      | `NOT LIKE`    | 反模糊匹配     | `"nameNotLike": "mike"` / `"name_not_like": "mike"`     | String                      |
| `In` / `_in`                 | `IN`          | IN 查询     | `"stateIn": [1, 2, 3]` / `"state_in": [1, 2, 3]`        | List/Array (String, Number) |
| `NotIn` / `_not_in`          | `NOT IN`      | NOT IN 查询 | `"stateNotIn": [1, 2, 3]` / `"state_not_in": [1, 2, 3]` | List/Array (String, Number) |
| `IsNull` / `_is_null`        | `IS NULL`     | 为空        | `"nameIsNull": true` / `"name_is_null": true`           | Boolean (true)              |
| `IsNotNull` / `_is_not_null` | `IS NOT NULL` | 不为空       | `"nameIsNotNull": true` / `"name_is_not_null": true`    | Boolean (true)              |
| `BitIn` / `_bit_in`          | `& =`         | 位运算 (包含)  | `"permissionBitIn": 4` / `"permission_bit_in": 4`       | Number                      |
| `BitNot` / `_bit_not`        | `& = 0`       | 位运算 (不包含) | `"permissionBitNot": 4` / `"permission_bit_not": 4`     | Number                      |

#### 入参示例

查询`name`包含`mike`, `version`为`1`, `age`在`18-60`之间, `state`为`1`或`2`或`3`数据:

```json
{
  "nameLike": "mike",
  "version": 1,
  "ageGe": 18,
  "ageLt": 60,
  "stateIn": [1, 2, 3]
}
```

#### 自定义后缀映射
- 修改`SuffixProcessor`的默认后缀, 来改变默认的后缀映射
- 重写`BoosterEngine`验证调用的方法, 改变指定实例的后缀映射
- 创建`SqlHelper<T>`时, 调用`process()`处理方法, 处理单次映射
- 可用操作符见[后缀映射表](#后缀映射表)

全局修改示例:
```java
import io.github.luminion.sqlbooster.model.sql.helper.processor.SuffixProcessor;

@SpringBootApplication
public class App {
    
    public static void main(String[] args) throws Exception {
        SpringApplication.run(App.class, args);

        // 指定后缀和操作符的映射关系
        HashMap<String, String> map = new HashMap<String, String>(); 
        map.put("_like", "LIKE");
        map.put("_ge", ">=");
        map.put("_le", "<=");
        map.put();
        map.put("_like", "LIKE");
        map.put("_not_eq", "<>");
        // 设置默认后缀映射
        SuffixProcessor.defaultSuffixMap(map);
    }
}
```

---

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
#### 指定字段检索条件
- 通过`conditions`字段指定查询条件,
- 其中每个条件对象`field`表示字段,`value`表示值,`operator`表示操作符号
- `operator`不填写时,默认为等于, 可选值(不区分大小写)：
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

查询`name`为`mike`, `version`大于等于`1`, `state`为`1`或`2`或`3`的数据
```json
{
  "conditions": [
    {
      "field": "name",
      "value": "mike"
    },
    {
      "field": "version",
      "operator": ">=",
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
#### 指定排序字段
- 通过`sorts`字段指定排序字段, 
- 其中每个条件对象`field`表示排序的字段,`isDesc`表示是否倒序(未指定时默认升序)

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
#### 复杂条件拼接
SqlHelper完整结构
- `conditions` - 查询条件
- `sorts` - 排序字段, 仅根节点有效
- `connector` - 条件间的连接符号, `AND`或`OR`, 不指定时默认`AND`
- `child` - 子节点, 一般用于组合嵌套`OR`条件
  - `conditions` - 子节点查询条件
  - `connector` - 子节点条件间的连接符号, `AND`或`OR`, 不指定时默认`AND`
  - `child` - 子子节点(可重复嵌套)

使用建议:
- 根节点的`conditions`字段用于组合`AND`条件 
- 当需要组合`OR`条件时, 将`OR`条件组合在`child`中
- `connector`默认为`AND`,不组合`OR`条件时无需传递
- `child`不使用时, 无需传递
- 
```json
{
  "conditions": [],
  "sorts": [],
  "child": {
    "conditions": [],
    "connector": "OR",
    "child": {
      "conditions": [],
      "connector": "AND",
      "child": {
        "conditions": []
      }
    }
  }
}
```
查询 `version`大于`1`,`state`为`1`, `name`为`mike`或`john`, `age`小于`18`或大于`60`的数据
```sql
select * from sys_user where (version > 1 and state = 1) and (name = 'mike' or name = 'john') and (age < 18 or age > 60)
```
输入参数:
```json
{
  "conditions": [
    {
      "field": "version",
      "operator": ">",
      "value": 1
    },\n    {
      "field": "state",
      "value": 1
    }
  ],
  "child": {
    "connector": "OR",
    "conditions": [
      {
        "field": "name",
        "value": "mike"
      },
      {
        "field": "name",
        "value": "john"
      }
    ],
    "child": {
      "connector": "OR",
      "conditions": [
        {
          "field": "age",
          "operator": "<",
          "value": 18
        },
        {
          "field": "age",
          "operator": ">",
          "value": 60
        }
      ]
    }
  }
}
```

## 字段映射
默认字段映射规则为:
- 通过Mybatis-plus的配置和注解来获取字段和数据库列的映射关系
- 满足后缀查询时, 会自动去掉后缀并转化为对应类型查询
- 若后缀查询和字段冲突, 则使用字段映射关系, 例如`nameLike`字段已存在时, 不会映射为`name`的模糊查询
- 若找不到对应的字段映射关系, 则会自动将字段放入`unmapped`中, 供后续处理
- 默认字段映射关系如下:
  - 获取实体类对应的表信息
  - 获取实体类字段信息
  - 获取`@TableField`注解的属性
  - 获取`EnhancedEntity`接口映射的属性

## 多表联查
支持以下方式查询非本表字段
- 自动映射, 兼容`动态SQL`和`动态后缀`查询
  - 通过`@TableField(exist = false, value="xxx")`注解, 将字段封装为指定数据表的指定列
  - 实现`EnhancedEntity`接口, 在`extraFieldColumnMap()`方法中定义字段名和数据库表/列的映射关系
- 在`mapper.xml`文件中自行手动指定

自动映射时, 需要在xml文件中添加需要连接的表和表名

### 通过`@TableFiled`指定

```java
public class SysUserVO {

  @TableField("user_name") // 字段为user_name
  private String userName;

  @TableField(exist = false, value = "role.name") // 映射为role表的name字段
  private String roleName;

  @TableField(exist = false, value = "dept.name") // 映射为dept表的name字段
  private String deptName;
}
``` 

### 实现EnhancedEntity接口

```java
public class SysUserVO implements EnhancedEntity {
  // 属性列表....
    
  @Override
  public Map<String, String> extraFieldColumnMap() {
    var map = new HashMap<Object, Object>();
    map.put("userName", "user_name"); // 将userName映射为实体类对应表的user_name字段
    map.put("roleId", "role.id"); // 将roleId映射为role表的id字段
    map.put("deptId", "dept.id"); // 将deptId映射为dept表的id字段
    return map;
  }
}
``` 

### 在`mapper.xml`文件中自行手动指定
所有不能自动映射的字段和值, 会作为`K`,`V`放入`param1.unmapped`中, 供后续处理, 可以在`mapper.xml`文件中自行手动指定, 如下:

```xml

<select id="voQueryByXml" resultType="com.example.test.vo.SysUserVO">
    SELECT a.* FROM
    sys_user a
    left join sys_role b on a.role_id = b.id
    left join sys_dept c on a.dept_id = c.id
    <where>
        <include refid="io.github.luminion.mybatisplus.enhancer.EnhancedMapper.queryFragment"/>
        <!--判断并字段是否存在值, 存在则添加条件-->
        <if test="param1.unmapped.roleName!=null">
            AND b.name = #{param1.unmapped.roleName}
        </if>
        <if test="param1.unmapped.deptName!=null">
            AND c.name = #{param1.unmapped.deptName}
        </if>
    </where>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="io.github.luminion.mybatisplus.enhancer.EnhancedMapper.sortFragment"/>
        <!--添加自定义排序条件-->
        , a.create_time DESC, a.id DESC
    </trim>
</select>
