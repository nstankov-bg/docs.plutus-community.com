import {
  Box,
  Button,
  Interpolation,
  useColorModeValue,
} from "@chakra-ui/react";
import { HamburgerIcon } from "@chakra-ui/icons";
import React, { createContext, useEffect, useState } from "react";
import useScreenWiderThanBreakpoint from "../../hooks/useScreenWiderThanBreakpoint";

export const NavigationContext = createContext({
  activeIndex: [0],
  setActiveIndex: (i: number[]): void => {},
});

export const Navigation: React.FC = ({ children }) => {
  const isWide = useScreenWiderThanBreakpoint(1024);
  const backgroundColor = useColorModeValue("gray.100", "gray.900");

  const collapseButtonBgColor = useColorModeValue("gray.100", "gray.700");

  const [activeIndex, setActiveIndex] = useState([0]);
  const [isCollapsed, setIsCollapsed] = useState(false);

  useEffect(() => {
    setIsCollapsed(!isWide);
  }, [isWide]);

  const positionStyle = isWide
    ? {
        position: "relative",
      }
    : {
        position: "absolute",
        left: 0,
      };

  return (
    <Box
      as="nav"
      m="0"
      pt="6"
      pl="2"
      pr="2"
      h="100%"
      background={backgroundColor}
      css={
        {
          display: "flex",
          flexDirection: "column",
          flexShrink: 0,
          boxSizing: "border-box",
          // boxShadow: shadow,
          overflowX: "visible",
          width: isCollapsed ? 0 : 256,
          transform: isCollapsed ? "translateX(-16px)" : "none",
          transition: "width 0.2s ease-in-out, transform 0.2s ease-in-out",
          ...positionStyle,
        } as Interpolation<{}>
      }
    >
      <Button
        title="collapse"
        css={{
          position: "absolute",
          top: 8,
          padding: 8,
          right: isCollapsed ? -56 : 4,
          zIndex: 99,
          boxShadow: isCollapsed ? "1px 0px 6px 1px rgba(0,0,0,0.2)" : "none",
        }}
        background={collapseButtonBgColor}
        onClick={() => setIsCollapsed((c) => !c)}
      >
        <HamburgerIcon />
      </Button>
      <Box overflowY="auto">
        <div
          css={{
            flex: 1,
            marginTop: 64,
            opacity: isCollapsed ? 0 : 1,
            transition: isCollapsed
              ? "opacity 0.1s ease-in-out"
              : "opacity 0.4s ease-in",
          }}
        >
          <NavigationContext.Provider value={{ activeIndex, setActiveIndex }}>
            {children}
          </NavigationContext.Provider>
        </div>
      </Box>
    </Box>
  );
};
