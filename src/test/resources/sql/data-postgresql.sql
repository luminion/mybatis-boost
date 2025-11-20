-- 如果存在，则删除现有的sys_user表
DROP TABLE IF EXISTS sys_user;

-- 创建sys_user表
CREATE TABLE sys_user
(
    "id"        int8         NOT NULL,
    name        VARCHAR(30)  NULL DEFAULT NULL,
    name_like   VARCHAR(255) NULL DEFAULT NULL,
    age         INT          NULL DEFAULT NULL,
    state       INT          NULL DEFAULT NULL,
    birth_date  DATE         NULL DEFAULT NULL,
    create_time TIMESTAMP    NULL DEFAULT NULL,
    update_time TIMESTAMP    NULL DEFAULT NULL,
    version     INT          NULL DEFAULT NULL,
    deleted     INT          NULL DEFAULT 0,
    description VARCHAR(255) NULL DEFAULT NULL,
    PRIMARY KEY (id)
);

-- 添加表和列的注释
COMMENT ON TABLE sys_user IS '系统用户表';
COMMENT ON COLUMN sys_user.id IS '主键ID';
COMMENT ON COLUMN sys_user.name IS '姓名';
COMMENT ON COLUMN sys_user.name_like IS '模糊查询姓名';
COMMENT ON COLUMN sys_user.age IS '年龄';
COMMENT ON COLUMN sys_user.state IS '状态';
COMMENT ON COLUMN sys_user.birth_date IS '出生日期';
COMMENT ON COLUMN sys_user.create_time IS '创建时间';
COMMENT ON COLUMN sys_user.update_time IS '更新时间';
COMMENT ON COLUMN sys_user.version IS '版本号';
COMMENT ON COLUMN sys_user.deleted IS '逻辑删除标志';
COMMENT ON COLUMN sys_user.description IS '描述';


-- 删除sys_user表中的所有现有数据
DELETE
FROM sys_user;

-- 向sys_user表中插入初始数据
INSERT INTO sys_user (id, name, name_like, age, state, birth_date, description, create_time, update_time, version,
                      deleted)
VALUES (1, '张三', '张三', 25, 1, '1998-01-01', 'EnhancedMapper测试用户1', NOW(), NOW(), 1, 0),
       (2, '李四', '李四', 30, 2, '1993-05-10', 'EnhancedMapper测试用户2', NOW(), NOW(), 1, 0),
       (3, '王五', '王五', 35, 3, '1988-12-20', 'EnhancedMapper测试用户3', NOW(), NOW(), 1, 0),
       (4, '赵六', NULL, 40, 4, '1985-03-15', 'EnhancedMapper测试用户4', NOW(), NOW(), 1, 0);