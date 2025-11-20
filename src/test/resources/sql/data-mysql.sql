-- noinspection SqlNoDataSourceInspectionForFile

DROP TABLE IF EXISTS sys_user;
CREATE TABLE sys_user
(
    id          BIGINT       NOT NULL AUTO_INCREMENT COMMENT '主键ID',
    name        VARCHAR(30)  NULL DEFAULT NULL COMMENT '姓名',
    name_like   VARCHAR(255) NULL DEFAULT NULL COMMENT '姓名Like后缀',
    age         INT          NULL DEFAULT NULL COMMENT '年龄',
    state       INT          NULL DEFAULT NULL COMMENT '状态',
    birth_date  DATE         NULL DEFAULT NULL COMMENT '生日',
    create_time DATETIME     NULL DEFAULT NULL COMMENT '创建时间',
    update_time DATETIME     NULL DEFAULT NULL COMMENT '更新时间',
    version     INT          NULL DEFAULT NULL COMMENT '版本号',
    deleted     INT          NULL DEFAULT 0 COMMENT '删除标志',
    description VARCHAR(255) NULL DEFAULT NULL COMMENT '描述',
    PRIMARY KEY (id)
) COMMENT = '系统用户表';

DELETE
FROM sys_user;

INSERT INTO sys_user (id, name, name_like, age, state, birth_date, description, create_time, update_time, version,
                      deleted)
VALUES (1, '张三', '张三', 25, 1, '1998-01-01', 'EnhancedMapper测试用户1', NOW(), NOW(), 1, 0),
       (2, '李四', '李四', 30, 2, '1993-05-10', 'EnhancedMapper测试用户2', NOW(), NOW(), 1, 0),
       (3, '王五', '王五', 35, 3, '1988-12-20', 'EnhancedMapper测试用户3', NOW(), NOW(), 1, 0),
       (4, '赵六', NULL, 40, 4, '1985-03-15', NULL, NOW(), NOW(), 1, 0);