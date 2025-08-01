# JSON Parameter Query Documentation

English | [中文](JSON_PARAMETER_ZH.md)

This document explains in detail how to use JSON format parameters for dynamic queries, which is one of the core features of mybatis-plus-enhancer.

## JSON Structure

The JSON parameter follows the following structure:

```json
{
  "conditions": [
    {
      "or": false,
      "field": "field name",
      "operator": "operator",
      "value": "value"
    }
  ],
  "child": {
    // nested conditions
  },
  "sorts": [
    {
      "field": "field name",
      "desc": true
    }
  ]
}
```

## Parameter Details

### conditions - Query Conditions List

conditions is an array containing multiple query condition objects.

#### Condition Object Properties:

- `or`: boolean type, indicates whether the relationship with other conditions is "OR", default is false (i.e., "AND" relationship)
- `field`: string type, represents the property name of the entity class
- `operator`: string type, represents the operator, default is "=", optional values include:
  - `=` (default)
  - `!=` or `<>`
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
- `value`: any type, represents the value corresponding to the property

#### Example:

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
      "value": "John"
    },
    {
      "field": "status",
      "operator": "in",
      "value": ["active", "pending"]
    }
  ]
}
```

### child - Nested Conditions

The child parameter supports nested conditions to implement complex query logic.

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
        "value": "John"
      },
      {
        "or": true,
        "field": "name",
        "operator": "like",
        "value": "Jane"
      }
    ]
  }
}
```

The above JSON is equivalent to SQL:
```sql
SELECT * FROM table 
WHERE age > 18 
  AND (name LIKE '%John%' OR name LIKE '%Jane%')
```

### sorts - Sort Conditions

sorts is an array containing multiple sort objects.

#### Sort Object Properties:

- `field`: string type, represents the property name of the entity class
- `desc`: boolean type, indicates whether to sort in descending order, default is false (ascending)

#### Example:

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

## Complete Example

### JSON Parameter Example

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
      "value": "John"
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

### Corresponding SQL

```sql
SELECT * FROM sys_user 
WHERE age > 18 
  AND name LIKE '%John%' 
  AND (id = 1 OR id = 2) 
ORDER BY age DESC, id ASC
```

## Usage

### Using in Controller

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

### Using in Service

```java
@Service
public class UserServiceImpl extends ServiceImpl<UserMapper, User> implements UserService {
    
    @Override
    public List<UserVO> voList(Object s) {
        // The s parameter can be a JSON object or Map
        return EnhancedService.super.voList(s);
    }
    
    @Override
    public IPage<UserVO> voPage(Object s, Long current, Long size) {
        // The s parameter can be a JSON object or Map
        return EnhancedService.super.voPage(s, current, size);
    }
}
```

## Special Operator Instructions

### IS NULL and IS NOT NULL

When using `IS NULL` or `IS NOT NULL` operators, the value can be any non-null value:

```json
{
  "conditions": [
    {
      "field": "email",
      "operator": "IS NULL",
      "value": ""  // The value can be any non-null value
    }
  ]
}
```

### IN and NOT IN

When using `IN` or `NOT IN` operators, the value must be an array:

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

When using the `LIKE` operator, the system will automatically add `%` before and after the value:

```json
{
  "conditions": [
    {
      "field": "name",
      "operator": "LIKE",
      "value": "John"  // Actual query is '%John%'
    }
  ]
}
```

## Notes

1. Field names must be property names that exist in the entity class
2. Operators are case-insensitive
3. When fields cannot be mapped to entity properties, they will be put into the unmapped parameters
4. Nested conditions can be nested in multiple layers to implement complex query logic
5. Sort conditions will be applied in the order of the array