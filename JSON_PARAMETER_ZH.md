# JSON参数查询说明

[English](JSON_PARAMETER.md) | 中文

本文档详细说明如何使用JSON格式参数进行动态查询，这是mybatis-plus-enhancer的核心功能之一。

## JSON结构

JSON参数遵循以下结构：

```json
{
  "conditions": [
    {
      "or": false,
      "field": "属性名",
      "operator": "运算符",
      "value": "值"
    }
  ],
  "child": {
    // 嵌套条件
  },
  "sorts": [
    {
      "field": "属性名",
      "desc": true
    }
  ]
}
```

## 参数详解

### conditions - 查询条件列表

conditions是一个数组，包含多个查询条件对象。

#### 条件对象属性：

- `or`: boolean类型，表示与其他条件的关系是否为"或者"，默认为false（即"并且"关系）
- `field`: string类型，表示实体类的属性名
- `operator`: string类型，表示运算符，默认为"="，可选值包括：
  - `=` (默认)
  - `!=` 或 `<>`
  - `>`
  - `>=`
  - `<=`
  - `<`
  - `LIKE`
  - `NOT LIKE`
  - `IS NULL`
  - `IS NOT NULL`
  - `IN`
  - `NOT IN`
- `value`: 任意类型，表示属性对应的值

#### 示例：

```json
{
  "conditions": [
    {
      "field": "age",
      "operator": ">",
      "value": 18
    },
    {
      "or": true,
      "field": "name",
      "operator": "like",
      "value": "张"
    },
    {
      "field": "status",
      "operator": "in",
      "value": ["active", "pending"]
    }
  ]
}
```

### child - 嵌套条件

child参数支持嵌套条件，可以实现复杂的查询逻辑。

```json
{
  "conditions": [
    {
      "field": "age",
      "operator": ">",
      "value": 18
    }
  ],
  "child": {
    "conditions": [
      {
        "field": "name",
        "operator": "like",
        "value": "张"
      },
      {
        "or": true,
        "field": "name",
        "operator": "like",
        "value": "李"
      }
    ]
  }
}
```

上述JSON等价于SQL：
```sql
SELECT * FROM table 
WHERE age > 18 
  AND (name LIKE '%张%' OR name LIKE '%李%')
```

### sorts - 排序条件

sorts是一个数组，包含多个排序对象。

#### 排序对象属性：

- `field`: string类型，表示实体类的属性名
- `desc`: boolean类型，表示是否倒序，默认为false（正序）

#### 示例：

```json
{
  "sorts": [
    {
      "field": "createTime",
      "desc": true
    },
    {
      "field": "name"
    }
  ]
}
```

## 完整示例

### JSON参数示例

```json
{
  "conditions": [
    {
      "field": "age",
      "operator": ">",
      "value": 18
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

### 对应的SQL

```sql
SELECT * FROM sys_user 
WHERE age > 18 
  AND name LIKE '%张%' 
  AND (id = 1 OR id = 2) 
ORDER BY age DESC, id ASC
```

## 使用方法

### 在Controller中使用

```java
@RestController
@RequestMapping("/user")
public class UserController {
    
    @Autowired
    private UserService userService;
    
    @PostMapping("/list")
    public List<UserVO> getUserList(@RequestBody Map<String, Object> params) {
        return userService.voList(params);
    }
    
    @PostMapping("/page")
    public IPage<UserVO> getUserPage(@RequestBody Map<String, Object> params,
                                     @RequestParam(defaultValue = "1") Long current,
                                     @RequestParam(defaultValue = "10") Long size) {
        return userService.voPage(params, current, size);
    }
}
```

### 在Service中使用

```java
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {
    
    @Override
    public List<UserVO> voList(Object s) {
        // s 参数可以是 JSON 对象或 Map
        return EnhancedService.super.voList(s);
    }
    
    @Override
    public IPage<UserVO> voPage(Object s, Long current, Long size) {
        // s 参数可以是 JSON 对象或 Map
        return EnhancedService.super.voPage(s, current, size);
    }
}
```

## 特殊操作符说明

### IS NULL 和 IS NOT NULL

当使用`IS NULL`或`IS NOT NULL`操作符时，value可以是任意非null值：

```json
{
  "conditions": [
    {
      "field": "email",
      "operator": "IS NULL",
      "value": ""  // 值可以是任意非null值
    }
  ]
}
```

### IN 和 NOT IN

当使用`IN`或`NOT IN`操作符时，value必须是数组：

```json
{
  "conditions": [
    {
      "field": "status",
      "operator": "IN",
      "value": ["active", "pending", "suspended"]
    }
  ]
}
```

### LIKE

使用`LIKE`操作符时，系统会自动在值的前后添加`%`：

```json
{
  "conditions": [
    {
      "field": "name",
      "operator": "LIKE",
      "value": "张"  // 实际查询为 '%张%'
    }
  ]
}
```

## 注意事项

1. 字段名必须是实体类中存在的属性名
2. 操作符不区分大小写
3. 当字段无法映射到实体属性时，会被放入unmapped参数中
4. 嵌套条件可以多层嵌套，实现复杂的查询逻辑
5. 排序条件会按照数组顺序依次应用