# mybatis-plus-enhancer
enhancer of mybatis-plus
* 针对mybatis-plus的增强
* 动态Sql工具
* IService增强
* BaseMapper增强
* Excel导入导出

原生成器相关代码已独立到单独仓库 https://github.com/bootystar/mybatis-plus-generator

## Maven依赖
```xml
<dependency>
    <groupId>io.github.bootystar</groupId>
    <artifactId>mybatis-plus-enhancer</artifactId>
    <version>1.2.0</version>
</dependency>
```

### 如果需要使用Excel功能,请自行引入FastExcel依赖
```xml
<!-- (可选)Excel导入导出 -->
<dependency>
    <groupId>cn.idev.excel</groupId>
    <artifactId>FastExcel</artifactId>
    <version>1.1.0</version>
</dependency>
```

### 中央仓库地址
拉取SNAPSHOT或镜像仓库尚未同步的RELEASE版本时可配置在`pom.xml`文件中
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
    <repository>
      <id>release</id>
      <name>release</name>
      <url>https://s01.oss.sonatype.org/content/repositories/releases/</url>
    </repository>
</repositories>
```
#### 若使用阿里云镜像拉取SNAPSHOT版本, 需在maven的`settings.xml`文件中配置`!snapshots`
```xml
<mirror>
  <id>aliyunmaven</id>
  <mirrorOf>*,!snapshots</mirrorOf>
  <name>aliyun</name>
  <url>https://maven.aliyun.com/repository/public</url>
</mirror>
```

## 快速开始
* 使`Mapper`实现`DynamicMapper`接口, 指定泛型为数据展示的实体类
* 用`MapperHelper`的`getSelectContent`方法,获取动态sql的xml内容
* 将动态sql的内容复制在`Mapper`对应的xml文件中
* (非必须)`Service`实现`DynamicSqlService`, 即会自动添加Mapper对应方法

继承示例:
```java
// 继承DynamicMapper, 指定泛型为数据展示的类(也可直接使用实体类)
public interface SysUserMapper extends BaseMapper<SysUser>, DynamicMapper<SysUserVO> {
}
```
获取xml内容示例:
```java
    void test1(){
        // 获取mapper对应的xml内容并复制到对应的xml文件中
        String xmlContent = MapperHelper.getSelectContent(SysUserMapper.class);
        System.out.println(xmlContent);
    }
```

使用示例:
```java
    @Resource
    private ISysUserService baseService;
    
    public void example() {
        // 条件(可以是实体类, 也可以是map)
        HashMap<String, Object> map = new HashMap<>();
        map.put("name","张三"); // 姓名= 张三
        map.put("age", 2); // 年龄= 2

        // 主要增强方法
        baseService.voById("1");// 根据id查询VO
        baseService.voByDTO(map); // 根据条件查询单个VO
        baseService.voList(map); // 根据条件查询列表
        baseService.voPage(map, 1L, 10L); // 根据条件查询分页
        OutputStream outputStream = new FileOutputStream("a.xlsx");
        baseService.excelExport(map, new FileOutputStream("a.xlsx"),SysUserVO.class); // 导出excel
        baseService.excelImport(new FileInputStream("a.xlsx"),SysUserVO.class); // 导入excel
  
        // 根据指定实体类\map\SqlHelper创建sqlHelper
        SqlHelper<SysUser> sqlHelper = SqlHelper.<SysUser>of(map);
        List<SysUserVO> vos1 = baseService.voList(sqlHelper); // 通过sqlHelper作为参数传入DynamicService进行查询
      
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

## SqlHelper条件辅助器
该类可直接作为前端入参DTO, 允许前端通过传参进行sql拼接

#### 入参格式示例:
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

## `DynamicService<V>`
该接口针对`IService`定义了一系列增强方法,`V`为VO数据展示类  
该接口大多方法都提供了`默认实现`, 实际需要实现的仅有`voSelect()`方法
* `getVOClass()`获取VO数据展示类
* `toVO()`将指定对象转化为`V`
* `toId()`获取实体类对象的`主键id`
* `insertByDTO()`新增方法, 默认返回值为新增数据的`主键id`
* `updateByDTO()`更新方法
* `voById()`根据`主键id`查询单个VO
* `voByDTO()`查询单个VO
* `voList()`查询VO列表
* `voPage()`查询VO分页
* `excelTemplate()`excel导入模板
* `excelImport()`excel文件导入
* `excelExport()`excel文件导出
### 提供2个默认实现
* `DynamicSqlService`
  * [DynamicSqlService.java](src/main/java/io/github/bootystar/mybatisplus/enhancer/core/support/DynamicSqlService.java)
  * 允许动态拼接sql的实现, 并针对传参进行防注入
  * 自动映射实体类属性字段
  * 非实体类属性会存放到`param1.map`中, 可通过param1.map.xxx判断参数是否存在,并添加对应逻辑
* `DynamicFieldService`
  * [DynamicFieldService.java](src/main/java/io/github/bootystar/mybatisplus/enhancer/core/support/DynamicFieldService.java)
  * 允许动态拼接sql的实现, 并对非本表字段/特殊值进行防注入
  * 允许后缀查询, 通过在原有字段名添加不同后缀,自动映射不同查询
  * 支持通过重写`getSuffixBuilder()`方法自定义后缀词
  * 自动映射实体类属性字段和对应后缀字段
  * 未自动映射的属性会存放到`param1.map`中, 可通过param1.map.xxx判断参数是否存在,并添加对应逻辑
  * 后缀示例:
    * name 精准查询
    * nameLike 模糊查询
    * ageLt 年龄小于
    * ageGt 年龄大于
    * ageIn 年龄在...内
* 也可根据自己的需求, 自定义实现`DynamicService`接口, 并实现`voSelect()`方法

## `DynamicMapper<V>`
该接口定义了动态mapper的入参查询,`V`为VO数据展示类
* 子mapper继承该类, 即可运行, 无需实现方法
* 需要提供对应xml文件, 可通过工具类获取xml文件内容
* 可在`mapper.xml`文件中添加对应的额外表及字段检索等自定义逻辑

### xml中额外SQL编写
* 在`xml`文件中, 可根据自身需要进行连表或者字段检索
* 基础表别名固定为`a`, 请勿修改
* `dynamicSelect`为自动映射封装的查询条件
* `dynamicSelect`下方添加额外条件(添加条件时不需要添加`WHERE`关键字)
* `dynamicSelect`下处添加额外条件时, 建议始终添加`AND`或`OR`连接符, 系统会自动去除多余的连接符
* 无法自动映射的查询条件会统一存放到`param1.map`中, 可通过param1.map.xxx判断参数是否存在,并添加对应逻辑
* 无法自动映射的查询条件值为`null`时, 系统会将字符串`"null"`作为值添加到map中,避免`<if test"param1.map.xxx!=null">`判断失效
* `dynamicSort`为自动映射封装的排序条件
* `dynamicSort`上方及下方可添加额外排序条件(添加条件时不需要添加`ORDER BY`关键字)
* `dynamicSort`下方添加排序时, 建议始终添加`,`连接符, 系统会自动去除多余的连接符
* 参数映射顺序`实体类属性字段信息`->`@TableFiled注解`->`DynamicEntity映射`

#### 工具类生成的xml基础内容示例
```xml
<select id="voSelectByXml" resultType="io.github.bootystar.vo.SysUserVO">
    SELECT
    a.*
    FROM
    sys_user a
    <!--额外添加连表-->
    left join sys_role b on a.role_id = b.id
    <where>
        <include refid="io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper.dynamicSelect"/>
        <!--添加查询条件-->
        AND a.deleted = 0
        <!--对未自动映射的条件进行判断, 并操作-->
        <if test="param1.map.keyword!=null">
            AND a.name = #{param1.map.keyword}
        </if>
    <where/>
    <trim prefix="ORDER BY" prefixOverrides=",">
        <include refid="io.github.bootystar.mybatisplus.enhancer.core.DynamicMapper.dynamicSort"/>
        <!--添加额外排序-->
        , a.create_time DESC , a.id DESC
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