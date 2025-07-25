package com.example.test.entity;

import com.baomidou.mybatisplus.annotation.*;
import lombok.Data;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * 用户
 *
 * @author bootystar
 * @since 2025-07-24
 * @see com.example.test.mapper.SysUserMapper
 */
@Data
@TableName("sys_user")
public class SysUser {

    /**
     * id
     */
    @TableId(value = "id", type = IdType.ASSIGN_ID)
    private Long id;

    /**
     * 逻辑删除标志
     */
    @TableLogic
    private Integer deleted;

    /**
     * 版本号
     */
    @Version
    private Integer version;

    /**
     * 创建时间
     */
    private LocalDateTime createTime;

    /**
     * 更新时间
     */
    private LocalDateTime updateTime;

    /**
     * 姓名
     */
    private String name;

    /**
     * 姓名Like后缀
     */
    private String description;

    /**
     * 年龄
     */
    private Integer age;

    /**
     * 生日
     */
    private LocalDate birthDate;

    /**
     * 状态
     */
    private Integer state;
}
