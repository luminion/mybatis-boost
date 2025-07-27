package com.example.test.impl;

import com.baomidou.dynamic.datasource.annotation.DS;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import com.example.test.entity.SysUser;
import com.example.test.mapper.SysUserMapper;
import com.example.test.vo.SysUserVO;
import io.github.bootystar.mybatisplus.enhancer.DynamicService;
import org.springframework.stereotype.Service;

/**
 * 用户服务实现类
 *
 * @author bootystar
 */
@Service
@DS("mysql")
public class SysUserDynamicFieldService extends ServiceImpl<SysUserMapper, SysUser> implements DynamicService<SysUserVO> {

  

}